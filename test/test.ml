type result = Decoded of Yojson.Basic.t | Diagnostic of string

type test = {
  cbor : string;
  roundtrip : bool;
  result : result;
}

let expected_failures = [10; 11; 12; 13; 47; 48; 49; 50; 51; 52; 71]

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
      { cbor; roundtrip; result }
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

let () =
  match List.tl @@ Array.to_list Sys.argv with
  | [] ->
    eprintfn "E: no test file given";
    exit 2
  | files ->
    eprintfn "I: running tests from %s" (String.concat ", " files);
    let tests =
      List.fold_left
        (fun all_tests file -> all_tests @ read file)
        []
        files
    in
    eprintfn "I: total tests = %d" (List.length tests);
    tests |> List.fold_left (fun (successes, warnings, failures) test ->
      let nr = successes + warnings + failures in
      let report success s =
        let ignore = List.mem nr expected_failures in
        let status, tally =
          match success, ignore with
          | true, _ -> "I:", (successes + 1, warnings, failures)
          | false, true -> "W: ignoring", (successes, warnings + 1, failures)
          | false, false -> "E:", (successes, warnings, failures +1)
        in
        eprintfn "%s test %d: %s" status nr s;
        tally
      in
      let test () =
        let cbor = CBOR.Simple.decode test.cbor in
        let diag = CBOR.Simple.to_diagnostic cbor in
        match test.result with
        | Diagnostic s when s = diag ->
            report true @@ Printf.sprintf "expected and got %s" s
        | Diagnostic s ->
            report false @@ Printf.sprintf "expected %s, got %s" s diag
        | Decoded json ->
            let json' = json_of_cbor cbor in
            if json = json' then
              report true @@ Printf.sprintf "expected and got %s"
                (Yojson.Basic.to_string json)
            else
              report false @@ Printf.sprintf "expected %s, got %s, aka %s"
                (Yojson.Basic.to_string json) (Yojson.Basic.to_string json') diag
      in
      try test () with exn -> report false @@ Printexc.to_string exn
    ) (0, 0, 0)
    |> (fun (successes, warnings, failures) ->
        eprintfn "I: finished. tests ok = %d failed = %d ignored = %d"
          successes failures warnings
    )
