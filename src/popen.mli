exception MentosError of string
val start : ?pygments_path:string -> unit -> unit
val is_alive : unit -> bool
val stop : string -> unit
val mentos :
  ?args:Yojson.Basic.json list ->
  ?kwargs:(string * Yojson.Basic.json) list ->
  ?code:string option -> string -> Yojson.Basic.json
val styles : unit -> Yojson.Basic.json
val filters : unit -> Yojson.Basic.json
val highlight : ?opts:(string * Yojson.Basic.json) list -> string -> string
val pygmentize : ?format:string -> lang:string -> string -> string
