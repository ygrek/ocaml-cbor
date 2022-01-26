type result = Decoded of Yojson.Basic.t | Diagnostic of string

type test = {
  cbor : string;
  roundtrip : bool;
  result : result;
  noncanonical : bool;
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
    Bytes.set r i @@ Char.chr @@ int_of_string ("0x" ^ String.sub s (i*2) 2)
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
      let noncanonical = try
          Util.to_bool @@ List.assoc "noncanonical" a
        with Not_found -> false
      in
      { cbor; roundtrip; result; noncanonical }
    | _ -> assert false
    end

let rec json_of_cbor : CBOR.Simple.t -> Yojson.Basic.t = function
| (`Null | `Bool _ | `Int _ | `Float _ as x) -> x
| `Undefined | `Simple _ -> `Null
| `Bytes x -> `String x
| `Text x -> `String x
| `Array x -> `List (List.map json_of_cbor x)
| `Map x -> `Assoc (List.map (fun (k,v) ->
  match k with
  | `Text s -> s, json_of_cbor v
  | _ -> fail "json_of_cbor: expected string key") x)
| `Tag (t, v) -> `String (Printf.sprintf "%d(%s)" t
                            (Yojson.Basic.to_string (json_of_cbor v)))

let rec main total_failed files =
  match files with
  | file::files ->
    eprintfn "I: running tests from %s" file;
    let tests = read file in
    eprintfn "I: total tests = %d" (List.length tests);
    let ok = ref 0 in
    let failed = ref 0 in
    let ignored = ref 0 in
    let nr = ref (-1) in
    let noncanonical = function
      | 11 | 13 | 47 | 48 | 49 | 50 | 51 | 52
      | 71 | 72 | 73 | 74 | 75 | 76 | 77 | 78 | 79 | 80 | 81 ->
        file = "appendix_a.json" (* XXX: ugly hack *)
      | _ -> false
    in
    tests |> List.iteri begin fun idx test ->
      try
        incr nr;
        let cbor = CBOR.Simple.decode test.cbor in
        let diag = CBOR.Simple.to_diagnostic cbor in
        let () = match test.result with
        | Diagnostic s ->
          if s <> diag then fail "expected %s, got %s" s diag
        | Decoded json ->
          let json' = json_of_cbor cbor in
          if json <> json' then fail "expected %s, got %s, aka %s"
            (Yojson.Basic.to_string json) (Yojson.Basic.to_string json') diag
        in
        if test.noncanonical || noncanonical idx then
          try let cbor = CBOR.Ctap2_canonical.decode test.cbor in
            fail "expected reject noncanonical CBOR, got %s"
              (CBOR.Simple.to_diagnostic (CBOR.Ctap2_canonical.to_simple cbor))
          with CBOR.Ctap2_canonical.Noncanonical _ -> incr ok
        else
          let cbor = CBOR.Ctap2_canonical.decode test.cbor in
          let diag = CBOR.Simple.to_diagnostic (CBOR.Ctap2_canonical.to_simple cbor) in
          let () = match test.result with
            | Diagnostic s ->
              if s <> diag then fail "expected %s, got %s" s diag
            | Decoded json ->
              let json' = json_of_cbor (CBOR.Ctap2_canonical.to_simple cbor) in
              if json <> json' then fail "expected %s, got %s, aka %s"
                  (Yojson.Basic.to_string json) (Yojson.Basic.to_string json') diag
          in
          incr ok

      with exn ->
        let ignore = file = "appendix_a.json" (* also ugly hack *) && List.mem !nr [10; 11; 12; 13; 71] in
        eprintfn "%s test %d: %s"
          (if ignore then "W: ignoring" else "E:") !nr (match exn with Failure s -> s | _ -> Printexc.to_string exn);
        incr (if ignore then ignored else failed)
    end;
    eprintfn "I: finished. tests ok = %d failed = %d ignored = %d" !ok !failed !ignored;
    main (total_failed + !failed) files
  | [] ->
    exit (if total_failed = 0 then 0 else 1)

let () =
  match List.tl @@ Array.to_list Sys.argv with
  | _ :: _ as files -> main 0 files
  | [] ->
    eprintfn "E: no test file given";
    exit 2
