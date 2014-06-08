
type result = Decoded of Yojson.Basic.json | Diagnostic of string

type test = {
  cbor : string;
  roundtrip : bool;
  result : result;
}

let (@@) f x = f x
let (|>) x f = f x
let eprintfn fmt = Printf.ksprintf prerr_endline fmt
let fail fmt = Printf.ksprintf failwith fmt

let of_hex s =
  assert (String.length s mod 2 = 0);
  let n = String.length s / 2 in
  let r = Bytes.create n in
  for i = 0 to pred n do
    r.[i] <- Char.chr @@ int_of_string ("0x" ^ String.sub s (i*2) 2)
  done;
  Bytes.to_string r

let read file =
  let open Yojson.Basic in
  Yojson.Safe.from_file file (* large ints *)
  |> Yojson.Safe.to_basic
  |> Util.to_list
  |> List.map begin function
    | `Assoc a ->
      let cbor = of_hex @@ Util.to_string @@ List.assoc "hex" a in
      let roundtrip = Util.to_bool @@ List.assoc "roundtrip" a in
      let result =
        try
          Diagnostic (Util.to_string @@ List.assoc "diagnostic" a)
        with Not_found -> 
          Decoded (List.assoc "decoded" a)
      in
      { cbor; roundtrip; result }
    | _ -> assert false
    end

let rec json_of_cbor : CBOR.Simple.t -> Yojson.Basic.json = function
| (`Null | `Bool _ | `Int _ | `Float _ as x) -> x
| `Undefined | `Simple _ -> `Null
| `Bytes x -> `String x
| `Text x -> `String x
| `Array x -> `List (List.map json_of_cbor x)
| `Map x -> `Assoc (List.map (fun (k,v) ->
  match k with
  | `Text s -> s, json_of_cbor v
  | _ -> fail "json_of_cbor: expected string key") x)

let () =
  match List.tl @@ Array.to_list Sys.argv with
  | file::[] ->
    eprintfn "I: running tests from %s" file;
    let tests = read file in
    eprintfn "I: total tests = %d" (List.length tests);
    let ok = ref 0 in
    let failed = ref 0 in
    tests |> List.iteri begin fun i test ->
      try
        let cbor = CBOR.Simple.decode test.cbor in
        let () = match test.result with
        | Diagnostic s ->
          let s' = CBOR.Simple.to_diagnostic cbor in
          if s <> s' then fail "expected %S, got %S" s s'
        | Decoded json ->
          let json' = json_of_cbor cbor in
          if json <> json' then fail "expected %s, got %s" (Yojson.Basic.to_string json) (Yojson.Basic.to_string json')
        in
        incr ok
      with exn ->
        eprintfn "E: test %d: %s" i (Printexc.to_string exn);
        incr failed
    end;
    eprintfn "I: finished. tests ok = %d failed = %d" !ok !failed;
    exit (if !ok = List.length tests then 0 else 1)
  | _ ->
    eprintfn "E: no test file given";
    exit 2
