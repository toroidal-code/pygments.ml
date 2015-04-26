open Core.Std
open Re2.Std
module JSON = Yojson.Basic

exception Json_error = Yojson.Json_error
exception MentosError of string

let _ = Random.self_init ();;

(**
 * Pygments provides acess to the Pygments library via a pipe and a long-running
 * Python process. *)

(* I really don't know a better way to deal with instance variables than
 * either global refs or actually using the object system. *)
let python_channels : Unix.Process_channels.t option ref = ref None;;

(** Detect a suitable Python binary to use  *)
let python_binary () =
  let result : string ref = ref "" in
  let in_chan = Unix.open_process_in "which python2" in
  result := String.strip @@ In_channel.input_all in_chan;
  ignore @@ Unix.close_process_in in_chan;
  !result

(* Generates an ISO-8601 formatted timestamp *)
let iso8601 (time:Time.t) =
  let open Time.Span.Parts in
  let uos = Time.Span.to_parts @@ Time.utc_offset ~zone:(Time.Zone.local) time in
  Format.sprintf "%s-%i:%02i" (Time.format time "%Y-%m-%02dT%02H:%02M:%02S" ~zone:(Time.Zone.local)) uos.hr uos.min

(** 
 *Get things started by opening a pipe to mentos (the freshmaker), a
 *Python process that talks to the Pygments library. We'll talk back and
 *forth across this pipe. *)
let start ?(pygments_path="/usr/lib/python2.7/site-packages/pygments/") () =
  Unix.putenv ~key:"PYGMENTS_PATH" ~data:pygments_path;
  let proc_chan = Unix.open_process_full "mentos.py" ~env:(Unix.environment ())  in
  python_channels := Some proc_chan


(**
 * Check for a proc_info.pid, and then hit `kill -0` with the pid to
 * check if the pid is still in the process table. If this function
 * gives us an ENOENT or ESRCH, we can also safely return false (no process
 * to worry about). Defensively, if EPERM is raised, in a odd/rare
 * dying process situation (e.g., mentos is checking on the pid of a dead
 * process and the pid has already been re-used) we'll want to raise
 * that as a more informative Mentos exception.
 *
 * Returns true if the child is alive. *)
let is_alive () =
  Option.is_some !python_channels

(**
 * Stop the child process by issuing a kill -9.
 *
 * We then call waitpid with the pid, which waits for that particular
 * child and reaps it.
 *
 * kill can set errno to ESRCH if, for some reason, the file
 * is gone; regardless the final outcome of this method
 * will be to set our @pid variable to nil.
 *
 * Technically, kill can also fail with EPERM or EINVAL (wherein
 * the signal isn't sent); but we have permissions, and
 * we're not doing anything invalid here. *)
let stop reason =
  print_endline reason;
  match !python_channels with
  | None -> ()
  | Some chans ->
      let open Unix.Process_info in
      ignore @@ Unix.close_process_full chans; (* TODO: Handle these errors properly *)
      python_channels := None


(* Originally from Rosetta Code, modified as necessary *)
let bin32_of_int d =
  if d < 0 then invalid_arg "bin32_of_int" else
  if d = 0 then String.init 32 ~f:(fun _ -> '0')  else
    let rec aux acc d =
      if d = 0 then acc else
        aux (string_of_int (d land 1) :: acc) (d lsr 1)
    in
    let pad_bin_string str =
      let len = String.length str in
      if len = 32 then str else
        (String.init (32 - len) ~f:(fun _ -> '0')) ^ str
    in
    pad_bin_string @@ String.concat ~sep:"" (aux [] d)


(* from StackOverflow 19884923 *)
exception Timeout

(** Run a given unary function, timing out
 ** after `timeout` seconds *)
let run_with_timeout timeout ~f ~args =
  let open Caml.Sys in
  let old_handler =
    signal sigalrm @@ Signal_handle (fun _ -> raise Timeout)
  in
  let finish () = ignore @@ Unix.alarm 0;
                  ignore @@ Caml.Sys.signal Caml.Sys.sigalrm old_handler
  in
  try ignore @@ Unix.alarm timeout;
      let res = (f args) in
      finish ();
      res
  with exn -> finish ();
              raise exn

(** With the code, prepend the id (with two spaces to avoid escaping weirdness if
 ** the following text starts with a slash (like terminal code), and append the
 ** id, with two padding also. This means we are sending over the 8 characters +
 ** code + 8 characters. *)
let add_ids id code =
  id ^ (Format.sprintf "  %s  %s" code id)

(** Write data to mentos, the Python process *)
let write_data ?(code=None) out_header =
  let open Unix.Process_channels in
  match !python_channels with
  | None -> ()
  | Some chans ->
      Out_channel.output_string chans.stdin out_header;
      Out_channel.flush chans.stdin;
      Core.Syslog.syslogf "[%s] Out header: %s" (iso8601 @@ Time.now ()) out_header;
      match code with
      | None -> ()
      | Some c -> Out_channel.output_string chans.stdin c;
                  Out_channel.flush chans.stdin


(** Return the final result for the API. Return OCaml things for the methods that
 ** want them, and text otherwise *)
let return_result res func =
  if func = "lexer_name_for" || func = "highlight" || func = "css" then
    `String res
  else
    match JSON.from_string res with
    | `List [`String s] -> `List [`String (String.rstrip s)]
    | `String s         -> `String (String.rstrip s)
    | res               -> res

(** Read the header from the pipe *)
let get_header () =
  let open Unix.Process_channels in
  let open Core.Syslog in
  let size_check size =
    let size_regex = Re2.create_exn "[0-1]{32}" in
    Re2.matches size_regex size
  in
  try
    let pc = Option.value_exn !python_channels in
    let size =
      let buf = Buffer.create 33 in
      Buffer.add_channel buf pc.stdout 33;
      Buffer.to_bytes buf
    in
    let size = String.drop_suffix size 1 in

    (* Sanity check the size *)
    if not (size_check size) then begin
      syslogf ~level:Level.ERR
        "[%s] Size returned from mentos.py invalid %s." (iso8601 @@ Time.now ()) size;
      stop "Size returned from mentos.py invalid.";
      raise (MentosError "Size returned from mentos.py invalid");
    end;

    (* Read the amount of bytes we should be expecting. We first 
     * convert the string of bits into an integer *)
    let header_bytes = (int_of_string @@ "0b" ^ size) + 1 in
    syslogf ~level:Level.INFO "[%s] Size in: %s (%i)" (iso8601 @@ Time.now ()) size header_bytes;
    let data =
      let buf = Buffer.create header_bytes in
      Buffer.add_channel buf pc.stdout header_bytes;
      Buffer.to_bytes buf
    in
    data
  with exn ->
    syslogf ~level:Level.ERR "[%s] Failed to get header." (iso8601 @@ Time.now ());
    stop "Failed to get header.";
    raise (MentosError "Failed to get header.")

(** Convert a text header into JSON for easy access. *)
let header_to_json header =
  let open Core.Syslog in
  syslogf ~level:Level.INFO "[%s] In header: %s" (iso8601 @@ Time.now ()) header;
  try JSON.from_string header
  with Json_error e ->
    syslogf ~level:Level.ERR "[%s] Failed to convert header to proper JSON object. Json_error: %s." (iso8601 @@ Time.now ()) e;
    stop e;
    raise (MentosError e)


(** Based on the header we receive, determine if we need
 ** to read more bytes, and read those bytes if necessary.
 **
 ** Then, do a sanity check wih the ids.
 **
 ** Returns a string result — either highlighted text or metadata. *)
let handle_header_and_return header id : string =
  let open Core.Syslog in
  if header <> "" then begin
    let header = match header_to_json header with
      | `Assoc h -> h
      | _ -> syslogf ~level:Level.ERR "[%s] Malformed header data." (iso8601 @@ Time.now ());
             stop "Malformed header data.";
             raise (MentosError "Malformed header data")
    in
    let bytes  = match ListLabels.assoc "bytes" header with `Int s -> s | _ -> assert false in

    (* Read more bytes (the actual response body) *)
    let res =
      let buf = Buffer.create bytes in
      Buffer.add_channel buf (Option.value_exn !python_channels).Unix.Process_channels.stdout bytes;
      Buffer.to_bytes buf
    in
    match ListLabels.assoc "method" header with
    | `String "highlight" ->

        (* Remove the newline from Python *)
        let res = String.drop_suffix res 1 in
        syslogf ~level:Level.INFO "[%s] Highlighting in progress" (iso8601 @@ Time.now ());

        (* Get the ids *)
        let start_id = String.slice res 0 8 in
        let end_id   = String.slice res (-8) 0 in

        (* Sanity check *)
        if not (start_id = id && end_id = id) then begin
          syslogf ~level:Level.ERR "[%s] IDs did not match. Aborting." (iso8601 @@ Time.now ());
          stop "IDs did not match. Aborting.";
          raise (MentosError "IDs did not match. Aborting.")
        end else begin
          (* We're good. Remove the padding *)
          let res = String.slice res 10 (-10) in
          syslogf ~level:Level.INFO "[%s] Highlighting complete." (iso8601 @@ Time.now ());
          res
        end
    | _ -> res

  end else begin
    syslogf ~level:Level.ERR "[%s] No header data back." (iso8601 @@ Time.now ());
    stop "No header data back.";
    raise (MentosError "No header received.")
  end
              
(** Our 'rpc'-ish request to mentos. Requires a method name, and then optional
 ** args, kwargs, and code. *)
let mentos ?(args=[]) ?(kwargs=[]) ?(code=None) func =
  try
    (* Open the pipe if necessary *)
    if not (is_alive ()) then start ();

    try
      (* Timeout requests that take too long.
       * Invalid MENTOS_TIMEOUT results in just using default. *)
      let timeout_time = int_of_string @@ Option.value ~default:"8" (Sys.getenv "MENTOS_TIMEOUT") in
      run_with_timeout timeout_time ~args:() ~f:begin fun _ ->
        let open Unix.Process_channels in

        (* For sanity checking on both sides of the pipe when highlighting, we prepend and
         * append an id. Mentos checks that these are 8 character ids and that they match.
         * It then returns the ids back to OCaml *)
        let id   = String.init 8 ~f:(fun _ -> char_of_int @@ 65 + (Random.int 25)) in
        let code = Option.map code ~f:(add_ids id) in

        (* Add metadat to the header and generate it *)
        let bytesize   = Bytes.length @@ Option.value code ~default:"" in

        (* Merge the kwargs with our values.
         * Unfortunately, OCaml has no sane alist processing functions,
         * so we have to convert both to String.Maps and then merge, 
         * and then convert back to an alist. Not pretty. *)
        let kwargs =
          String.Map.to_alist @@
          String.Map.merge
            ~f:(fun ~key -> function `Both(_,d) | `Left d | `Right d -> Some d)
            (String.Map.of_alist_exn kwargs)
            (String.Map.of_alist_exn [
                ("id", `String id);
                ("bytes",`Int bytesize)
              ])
        in

        (* The out header, formatted as json and then serialized to a string *)
        let out_header : string = JSON.to_string ~std:true @@
          `Assoc [
            ("args",   `List args);
            ("method", `String func);
            ("kwargs", `Assoc kwargs)
          ]
        in

        (* Get the size of the header itself and write that *)
        let bits = bin32_of_int (String.length out_header) in
        ignore @@ Out_channel.output_string (Option.value_exn !python_channels).stdin bits;
        Out_channel.flush (Option.value_exn !python_channels).stdin;

        (* Mentos is now waiting for the header, and, potentially, code. *)
        write_data ~code out_header;

        (* Mentos will now return data to us. First it sends the header *)
        let header = get_header () in

        (* Now handle the header, and read any more data required *)
        let res = handle_header_and_return header id in

        (* Finally, return what we got *)
        return_result res func;
      end
    with Timeout ->
      let open Core.Syslog in
      Core.Syslog.syslogf ~level:Core.Syslog.Level.ERR
        "[%s] Timeout on a mentos %s call" (iso8601 @@ Time.now ()) func;
      `Null 
  with Unix.Unix_error(Unix.EPIPE,_,_)
     | End_of_file -> stop "EPIPE";
                      raise (MentosError "EPIPE")

(* let formatters () = *)
(*   match mentos "get_all_formatters" with  *)
(*   List.fold (mentos "get_all_formatters") ~init:String.Map.empty ~f:(fun map (name,desc,aliases) -> *)
(*       let key = Str.global_replace (Str.regexp_string "Formatter$") "" name in *)
(*       let data = String.Map.of_alist_exn [ *)
(*           ("name",        name); *)
(*           ("aliases",     aliases); *)
(*           ("description", desc); *)
(*         ] *)
(*       in *)
(*       String.Map.add map ~key ~data *)
(*     ) *)

(* let lexers () = *)
(*   List.fold (mentos "get_all_formatters") ~init:String.Map.empty ~f:(fun map lexer -> *)
(*       let ln = List.nth_exn lexer in *)
(*       let key = List.hd_exn lexer in *)
(*       let data = String.Map.of_alist_exn [ *)
(*           ("name",      ln 0); *)
(*           ("aliases",   ln 1); *)
(*           ("filetypes", ln 2); *)
(*           ("mimetypes", ln 3); *)
(*         ] *)
(*       in  *)
(*       String.Map.add map ~key ~data *)
(*     ) *)

let styles ()  = mentos "get_all_styles"
let filters () = mentos "get_all_filters"

let highlight ?(opts=[]) code =
  if code = "" then
    code
  else match mentos "highlight" ~kwargs:opts ~code:(Some code) with
    | `String s -> s
    | e -> ignore @@ print_endline @@ JSON.to_string e; "ERROR"

let pygmentize ?(format="html") ~lang=
  highlight ~opts:[("lexer", `String lang); ("formatter", `String format)]
