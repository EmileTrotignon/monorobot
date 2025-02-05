open Printf
open Devkit
open Common
open Util

module Github : Api.Github = struct
  let commits_url ~(repo : Github_t.repository) ~sha =
    let _, url = ExtLib.String.replace ~sub:"{/sha}" ~by:("/" ^ sha) ~str:repo.commits_url in
    url

  let contents_url ~(repo : Github_t.repository) ~path =
    let _, url = ExtLib.String.replace ~sub:"{+path}" ~by:path ~str:repo.contents_url in
    url

  let pulls_url ~(repo : Github_t.repository) ~number =
    let _, url = ExtLib.String.replace ~sub:"{/number}" ~by:(sprintf "/%d" number) ~str:repo.pulls_url in
    url

  let issues_url ~(repo : Github_t.repository) ~number =
    let _, url = ExtLib.String.replace ~sub:"{/number}" ~by:(sprintf "/%d" number) ~str:repo.issues_url in
    url

  let compare_url ~(repo : Github_t.repository) ~basehead:(base, merge) =
    let _, url = ExtLib.String.replace ~sub:"{/basehead}" ~by:(sprintf "/%s...%s" base merge) ~str:repo.compare_url in
    url

  let build_headers ?token () =
    let headers = [ "Accept: application/vnd.github.v3+json" ] in
    Option.map_default (fun v -> sprintf "Authorization: token %s" v :: headers) headers token

  let prepare_request ~secrets ~(repo : Github_t.repository) url =
    let token = Context.gh_token_of_secrets secrets repo.url in
    let headers = build_headers ?token () in
    let url =
      match Context.gh_repo_of_secrets secrets repo.url with
      | None -> url
      | Some repo_config ->
        (* The url might have been built based on information received through an untrusted source such as a slack message.
           Normalizing it using trusted secrets. *)
        let repo_config_url_scheme = repo_config.url |> Uri.of_string |> Uri.scheme in
        url |> Uri.of_string |> flip Uri.with_scheme repo_config_url_scheme |> Uri.to_string
    in
    headers, url

  let get_resource ~secrets ~repo url =
    let headers, url = prepare_request ~secrets ~repo url in
    match%lwt http_request ~headers `GET url with
    | Ok res -> Lwt.return @@ Ok res
    | Error e -> Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to get resource from %s" e url

  let post_resource ~secrets ~repo body url =
    let headers, url = prepare_request ~secrets ~repo url in
    match%lwt http_request ~headers ~body:(`Raw ("application/json; charset=utf-8", body)) `POST url with
    | Ok res -> Lwt.return @@ Ok res
    | Error e -> Lwt.return @@ fmt_error "POST to %s failed : %s" url e

  let get_config ~(ctx : Context.t) ~repo =
    let secrets = Context.get_secrets_exn ctx in
    let url = contents_url ~repo ~path:ctx.config_filename in
    match%lwt get_resource ~secrets ~repo url with
    | Error e -> Lwt.return @@ fmt_error "error while querying remote: %s\nfailed to get config from file %s" e url
    | Ok res ->
      let response = Github_j.content_api_response_of_string res in
      (match response.encoding with
      | "base64" -> begin
        try
          response.content
          |> Re2.rewrite_exn (Re2.create_exn "\n") ~template:""
          |> decode_string_pad
          |> Config_j.config_of_string
          |> fun res -> Lwt.return @@ Ok res
        with exn ->
          let e = Exn.to_string exn in
          Lwt.return
          @@ fmt_error "error while reading config from GitHub response: %s\nfailed to get config from file %s" e url
      end
      | encoding ->
        Lwt.return
        @@ fmt_error "unexpected encoding '%s' in Github response\nfailed to get config from file %s" encoding url)

  let get_api_commit ~(ctx : Context.t) ~(repo : Github_t.repository) ~sha =
    let%lwt res = commits_url ~repo ~sha |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo in
    Lwt.return @@ Result.map Github_j.api_commit_of_string res

  let get_pull_request ~(ctx : Context.t) ~(repo : Github_t.repository) ~number =
    let%lwt res = pulls_url ~repo ~number |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo in
    Lwt.return @@ Result.map Github_j.pull_request_of_string res

  let get_issue ~(ctx : Context.t) ~(repo : Github_t.repository) ~number =
    let%lwt res = issues_url ~repo ~number |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo in
    Lwt.return @@ Result.map Github_j.issue_of_string res

  let get_compare ~(ctx : Context.t) ~(repo : Github_t.repository) ~basehead =
    let%lwt res = compare_url ~repo ~basehead |> get_resource ~secrets:(Context.get_secrets_exn ctx) ~repo in
    Lwt.return @@ Result.map Github_j.compare_of_string res

  let request_reviewers ~(ctx : Context.t) ~(repo : Github_t.repository) ~number ~reviewers =
    let body = Github_j.string_of_request_reviewers_req reviewers in
    let%lwt res =
      pulls_url ~repo ~number ^ "/requested_reviewers"
      |> post_resource ~secrets:(Context.get_secrets_exn ctx) ~repo body
    in
    Lwt.return @@ Result.map ignore res
end

module Slack : Api.Slack = struct
  let log = Log.from "slack"

  let slack_api_request ?headers ?body ?(log_res = false) meth url read =
    match%lwt http_request ?headers ?body meth url with
    | Error e -> Lwt.return_error (query_error_msg url e)
    | Ok s ->
      (match log_res with
      | true -> log#info "response from %s: %s" url (Yojson.Basic.prettify s)
      | false -> ());
      (match Slack_j.slack_response_of_string read s with
      | res -> Lwt.return res
      | exception exn -> Lwt.return_error (query_error_msg url (Exn.to_string exn)))

  let request_token_auth ~name ?headers ?body ?(log_res = false) ~ctx meth path read =
    log#info "%s: starting request" name;
    let secrets = Context.get_secrets_exn ctx in
    match secrets.slack_access_token with
    | None -> Lwt.return @@ fmt_error "%s: failed to retrieve Slack access token" name
    | Some access_token ->
      let headers = bearer_token_header access_token :: Option.default [] headers in
      let url = sprintf "https://slack.com/api/%s" path in
      (match%lwt slack_api_request ?body ~log_res ~headers meth url read with
      | Ok res -> Lwt.return @@ Ok res
      | Error e -> Lwt.return @@ fmt_error "%s: failure : %s" name e)

  let request_token_raw ~name ?(add_prefix = true) ?headers ?body ~ctx meth path =
    log#info "%s: starting request" name;
    let secrets = Context.get_secrets_exn ctx in
    match secrets.slack_access_token with
    | None -> Lwt.return @@ fmt_error "%s: failed to retrieve Slack access token" name
    | Some access_token ->
      let headers = bearer_token_header access_token :: Option.default [] headers in
      let url = if add_prefix then sprintf "https://slack.com/api/%s" path else path in
      (match%lwt http_request ?body ~headers meth url with
      | Ok res -> Lwt.return @@ Ok res
      | Error e -> Lwt.return @@ fmt_error "%s: failure : %s" name e)

  let read_unit s l =
    (* must read whole response to update lexer state *)
    ignore (Slack_j.read_ok_res s l)

  let lookup_user_cache = Hashtbl.create 50

  let lookup_user' ~(ctx : Context.t) ~(cfg : Config_t.config) ~email () =
    (* Check if config holds the Github to Slack email mapping  *)
    let email = List.assoc_opt email cfg.user_mappings |> Option.default email in
    let url_args = Web.make_url_args [ "email", email ] in
    match%lwt
      request_token_auth ~name:"lookup user by email" ~ctx `GET
        (sprintf "users.lookupByEmail?%s" url_args)
        Slack_j.read_lookup_user_res
    with
    | Error _ as e -> Lwt.return e
    | Ok user ->
      Hashtbl.replace lookup_user_cache email user;
      Lwt.return_ok user

  (** [lookup_user cfg email] queries slack for a user profile with [email] *)
  let lookup_user ?(cache : [ `Use | `Refresh ] = `Use) ~(ctx : Context.t) ~(cfg : Config_t.config) ~email () =
    match cache with
    | `Refresh -> lookup_user' ~ctx ~cfg ~email ()
    | `Use ->
    match Hashtbl.find_opt lookup_user_cache email with
    | Some user -> Lwt.return_ok user
    | None -> lookup_user' ~ctx ~cfg ~email ()

  let list_users ?cursor ?limit ~(ctx : Context.t) () =
    let cursor_option = Option.map (fun c -> "cursor", c) cursor in
    let limit_option = Option.map (fun l -> "limit", Int.to_string l) limit in
    let url_args = Web.make_url_args @@ List.filter_map id [ cursor_option; limit_option ] in
    request_token_auth ~name:"list users" ~ctx `GET (sprintf "users.list?%s" url_args) Slack_j.read_list_users_res

  let channel_list = ref None

  let list_channels ?cursor ?exclude_archived ?team_id ?limit ~(ctx : Context.t) () =
    let cursor_option = Option.map (fun c -> "cursor", c) cursor in
    let limit_option = Option.map (fun l -> "limit", Int.to_string l) limit in
    let exclude_archived_option = Option.map (fun l -> "exclude_archived", Bool.to_string l) exclude_archived in
    let team_id_option = Option.map (fun l -> "team_id", Bool.to_string l) team_id in
    let url_args =
      Web.make_url_args @@ List.filter_map id [ cursor_option; limit_option; exclude_archived_option; team_id_option ]
    in
    Lwt_result.map
      (fun Slack_t.{ channels } -> channels)
      (request_token_auth ~name:"list channels" ~ctx `GET
         (sprintf "conversations.list?%s" url_args)
         Slack_j.read_channel_list_res)

  let list_channels ?(cache : [ `Use | `Refresh ] = `Use) ~ctx () =
    match cache with
    | `Refresh -> list_channels ~ctx ()
    | `Use ->
    match !channel_list with
    | Some channels -> Lwt.return_ok channels
    | None -> list_channels ~ctx ()

  let channel_id_of_name ~(ctx : Context.t) (name : string) =
    let%lwt channels = list_channels ~ctx () in
    match channels with
    | Error _ as e -> Lwt.return e
    | Ok channels ->
    match
      List.find_opt
        (function
          | ({ name = Some name'; _ } : Slack_t.channel_list_res_elt) when name = name' -> true
          | _ -> false)
        channels
    with
    | None -> Lwt.return_error (sprintf "channel_id_of_name: channel with name %S not found" name)
    | Some { id; name = _ } ->
      log#info "channel_id_of_name: found channel %S has id %S" name (Slack_channel.Ident.project id);
      Lwt.return_ok id

  let channel_id_of_name ~ctx channel = channel |> Slack_channel.Any.project |> channel_id_of_name ~ctx

  let channel_id_of_name_opt ~ctx channel =
    match channel with
    | None -> Lwt.return_ok None
    | Some channel ->
      (match%lwt channel_id_of_name ~ctx channel with
      | Error _ as e -> Lwt.return e
      | Ok channel_id -> Lwt.return_ok (Some channel_id))

  module ChannelTbl = Hashtbl.Make (struct
    include Slack_channel.Ident
    let equal = Slack_channel.equal
    let hash = Slack_channel.hash
  end)

  let joined_channels : unit ChannelTbl.t = ChannelTbl.create 50

  let join_channel ~(ctx : Context.t) (channel : Slack_channel.Ident.t) =
    let data = Slack_j.(string_of_join_channel_req { channel }) in
    let body = `Raw ("application/json; charset=utf-8", data) in
    request_token_auth ~name:"join channel" ~ctx `POST ~body "conversations.join" Slack_j.read_ok_res

  let join_channel ~ctx channel =
    match ChannelTbl.find_opt joined_channels channel with
    | Some () -> Lwt.return_ok ()
    | None ->
      (match%lwt join_channel ~ctx channel with
      | Error _ as e -> Lwt.return e
      | Ok _ ->
        ChannelTbl.add joined_channels channel ();
        Lwt.return_ok ())

  let join_channel_opt ~ctx channel =
    match channel with
    | None -> Lwt.return_ok ()
    | Some channel -> join_channel ~ctx channel

  (** [send_notification ctx msg] notifies [msg.channel] with the payload [msg];
      uses web API with access token if available, or with webhook otherwise *)
  let send_notification ~(ctx : Context.t) ~(msg : Slack_t.post_message_req) =
    log#info "sending to %s" (Slack_channel.Any.project msg.channel);
    let build_error e = fmt_error "%s\nfailed to send Slack notification" e in
    let secrets = Context.get_secrets_exn ctx in
    let headers, url, webhook_mode =
      match Context.hook_of_channel ctx msg.channel with
      | Some url -> [], Some url, true
      | None ->
      match secrets.slack_access_token with
      | Some access_token -> [ bearer_token_header access_token ], Some "https://slack.com/api/chat.postMessage", false
      | None -> [], None, false
    in
    match url with
    | None ->
      Lwt.return
      @@ build_error
      @@ sprintf "no token or webhook configured to notify channel %s" (Slack_channel.Any.project msg.channel)
    | Some url ->
      let data = Slack_j.string_of_post_message_req msg in
      let body = `Raw ("application/json; charset=utf-8", data) in
      log#info "data: %s" data;
      if webhook_mode then begin
        match%lwt http_request ~body ~headers `POST url with
        | Ok _res ->
          (* Webhooks reply only 200 `ok`. We can't generate anything useful for notification success handlers *)
          Lwt.return @@ Ok None
        | Error e -> Lwt.return @@ build_error (query_error_msg url e)
      end
      else begin
        match%lwt slack_api_request ~body ~headers `POST url Slack_j.read_post_message_res with
        | Ok res -> Lwt.return @@ Ok (Some res)
        | Error e -> Lwt.return @@ build_error e
      end

  let send_chat_unfurl ~(ctx : Context.t) ~channel ~ts ~unfurls () =
    let req = Slack_j.{ channel; ts; unfurls } in
    let data = Slack_j.string_of_chat_unfurl_req req in
    request_token_auth ~name:"unfurl slack links"
      ~body:(`Raw ("application/json; charset=utf-8", data))
      ~ctx `POST "chat.unfurl" read_unit

  let send_auth_test ~(ctx : Context.t) () =
    request_token_auth ~name:"retrieve bot information" ~ctx `POST "auth.test" Slack_j.read_auth_test_res

  let get_thread_permalink ~(ctx : Context.t) (thread : State_t.slack_thread) =
    let url_args =
      Web.make_url_args
        [ "channel", Slack_channel.Ident.project thread.cid; "message_ts", Slack_timestamp.project thread.ts ]
    in
    match%lwt
      request_token_auth ~name:"retrieve message permalink" ~ctx `GET
        (sprintf "chat.getPermalink?%s" url_args)
        Slack_j.read_permalink_res
    with
    | Error (s : string) ->
      log#warn "couldn't fetch permalink for slack thread %s: %s" (Slack_timestamp.project thread.ts) s;
      Lwt.return_none
    | Ok (res : Slack_t.permalink_res) when res.ok = false ->
      log#warn "bad request fetching permalink for slack thread %s: %s" (Slack_timestamp.project thread.ts)
        (Option.default "" res.error);
      Lwt.return_none
    | Ok ({ permalink; _ } : Slack_t.permalink_res) -> Lwt.return_some permalink

  let get_upload_URL_external ~ctx ~filename ~alt_txt ~size =
    (* Check if config holds the Github to Slack email mapping  *)
    let url_args = Web.make_url_args [ "filename", filename; "alt_txt", alt_txt; "length", string_of_int size ] in
    request_token_auth ~name:"get_upload_URL_external" ~log_res:true ~ctx `GET
      (sprintf "files.getUploadURLExternal?%s" url_args)
      Slack_j.read_upload_url_res

  let post_file_content ~ctx ~upload_url ~filename:_ ~content =
    (* Out_channel.with_open_bin "/tmp/joblog" (fun oc -> output_string oc content); *)
    (*let upload_url =
      ignore upload_url;
      "central-sadly-hawk.ngrok-free.app/upload"
    in*)
    log#info "post_file_content: starting file upload to %s" upload_url;
    (* let headers = [ sprintf {|Content-Disposition: attachment; filename="%s"|} filename ] in *)
    let body = `Raw ("application/octet-stream", content) in
    (* let body = `Form [filename, content] in *)
    match%lwt
      request_token_raw ~name:"post_file_content" (*~headers*) ~add_prefix:false ~ctx ~body `POST upload_url
    with
    | Error e -> Lwt.return_error (query_error_msg upload_url e)
    | Ok str ->
      log#info "post_file_content: upload file successful: %S" str;
      Lwt.return_ok ()

  let complete_upload_external ~(ctx : Context.t) ?channel ?thread_ts ~file_id ~title ?initial_comment () =
    log#info "send file: complete_upload_external to channel: %S"
      (match channel with
      | None -> "None"
      | Some c -> Slack_channel.Any.project c);
    match%lwt channel_id_of_name_opt ~ctx channel with
    | Error _ as e -> Lwt.return e
    | Ok channel_id ->
      (match%lwt join_channel_opt ~ctx channel_id with
      | Error _ as e -> Lwt.return e
      | Ok () ->
        let files = [ ({ id = file_id; title } : Slack_t.complete_upload_external_file) ] in
        let thread_ts =
          ignore thread_ts;
          None
        in
        let req =
          {
            Slack_t.files;
            channel_id;
            channels = None;
            thread_ts;
            initial_comment;
          }
        in
        let data = Slack_j.string_of_complete_upload_external_req req in
        log#info "data: %s" data;
        let body = `Raw ("application/json; charset=utf-8", data) in
        Lwt_result.bind
          (request_token_auth ~name:"complete_upload_external" ~log_res:true ~ctx `POST ~body
             (sprintf "files.completeUploadExternal")
             Slack_j.read_complete_upload_external_res)
          (fun res ->
            let text =
              String.concat "\n"
                (List.map
                   (fun ({ url_private; url_private_download; permalink; permalink_public } :
                          Slack_t.complete_upload_external_file_res) ->
                     sprintf "private: %s private_download: %s permalink: %s, permalink_public: %s" url_private
                       url_private_download permalink permalink_public)
                   res.files)
            in
            let msg, _, _ = Slack.make_message ~text ~channel:(Option.get channel) () in
            Lwt_result.map (fun _ -> ()) (send_notification ~ctx ~msg)))

  let send_file ~(ctx : Context.t) ~(file : Slack.file_req) =
    log#info "send file: starting to send file %S" file.title;
    let { Slack.name; alt_txt; content; title; channel; initial_comment; thread_ts } = file in
    (*let content =
      ignore content;
      In_channel.with_open_bin "/tmp/joblog" (fun ic -> In_channel.input_all ic)
    in*)
    match%lwt get_upload_URL_external ~ctx ~filename:name ~alt_txt ~size:(String.length content) with
    | Error _ as e ->
      log#info "send file: failed to get upload URL for file %S" file.title;
      Lwt.return @@ e
    | Ok { Slack_t.upload_url; file_id } ->
      (match%lwt post_file_content ~ctx ~upload_url ~filename:name ~content with
      | Error _ as e ->
        log#info "send file: failed to post file content for file %S" file.title;
        Lwt.return e
      | Ok () -> complete_upload_external ~ctx ?channel ?thread_ts ~file_id ~title ?initial_comment ())
end

module Buildkite : Api.Buildkite = struct
  let log = Log.from "buildkite"

  module Builds_cache = Cache (struct
    type t = Buildkite_t.get_build_res
  end)

  (* 24h cache ttl and purge interval. We store so little data per build that it's not worth cleaning up entries sooner. *)
  let builds_cache = Builds_cache.create ~ttl:Builds_cache.default_purge_interval ()

  let buildkite_api_request ?headers ?body meth url read =
    match%lwt http_request ?headers ?body meth url with
    | Error e -> Lwt.return_error (query_error_msg url e)
    | Ok s ->
    match read s with
    | res -> Lwt.return_ok res
    | exception exn -> Lwt.return_error (query_error_msg url (Exn.to_string exn))

  let request_token_auth ~name ?headers ?body ~ctx meth path read =
    log#info "%s: starting request" name;
    let secrets = Context.get_secrets_exn ctx in
    match secrets.buildkite_access_token with
    | None -> Lwt.return @@ fmt_error "%s: failed to retrieve Buildkite access token" name
    | Some access_token ->
      let headers = bearer_token_header access_token :: Option.default [] headers in
      let url =
        if String.starts_with ~prefix:"https://api.buildkite.com/v2/" path then path
        else sprintf "https://api.buildkite.com/v2/%s" path
      in
      (match%lwt buildkite_api_request ?body ~headers meth url read with
      | Ok res -> Lwt.return @@ Ok res
      | Error e -> Lwt.return @@ fmt_error "%s: failure : %s" name e)

  let get_job_log ~ctx (job : Buildkite_t.job) =
    match job.log_url with
    | None -> Lwt.return_error "Unable to get job log, job has no log_url field"
    | Some log_url ->
      (match%lwt request_token_auth ~name:"get buildkite job logs" ~ctx `GET log_url Buildkite_j.job_log_of_string with
      | Ok logs -> Lwt.return_ok logs
      | Error e -> Lwt.return_error e)

  let get_build' ~ctx ~org ~pipeline ~build_nr map =
    let build_url = sprintf "organizations/%s/pipelines/%s/builds/%s" org pipeline build_nr in
    match%lwt request_token_auth ~name:"get build details" ~ctx `GET build_url Buildkite_j.get_build_res_of_string with
    | Ok build ->
      Builds_cache.set builds_cache build_nr build;
      Lwt.return_ok (map build)
    | Error e -> Lwt.return_error e

  let get_build ?(cache : [ `Use | `Refresh ] = `Use) ~(ctx : Context.t) (n : Github_t.status_notification) =
    match Util.Build.get_org_pipeline_build n with
    | Error e -> Lwt.return_error e
    | Ok (org, pipeline, build_nr) ->
    match cache with
    | `Use ->
      (match Builds_cache.get builds_cache build_nr with
      | Some build -> Lwt.return_ok build
      | None -> get_build' ~ctx ~org ~pipeline ~build_nr id)
    | `Refresh -> get_build' ~ctx ~org ~pipeline ~build_nr id

  let get_build_branch ~(ctx : Context.t) (n : Github_t.status_notification) =
    match Util.Build.get_org_pipeline_build n with
    | Error e -> Lwt.return_error e
    | Ok (org, pipeline, build_nr) ->
      let map_branch { Buildkite_t.branch; _ } : Github_t.branch = { name = branch } in
      (match Builds_cache.get builds_cache build_nr with
      | Some { Buildkite_t.branch; _ } -> Lwt.return_ok ({ name = branch } : Github_t.branch)
      | None ->
        log#info "Fetching branch details for build %s in pipeline %s" build_nr pipeline;
        get_build' ~ctx ~org ~pipeline ~build_nr map_branch)
end
