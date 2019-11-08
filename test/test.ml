type result = Decoded of Yojson.Basic.t | Diagnostic of string

type test = {
  cbor : string;
  roundtrip : bool;
  result : result;
}

module type Impl = sig
  include CBOR.Simple
  val expected_failures : int list
  val name : string
  val t_to_json_int : integer -> Yojson.Basic.json
end

module Impl : Impl = struct
  include CBOR.Simple
  let expected_failures = [10; 11; 12; 13; 47; 48; 49; 50; 51; 52; 71; 82]
  let name = "int"
  let t_to_json_int x =
    `Int x
end

module Impl64 : Impl = struct
  include CBOR.Simple64
  let expected_failures = [10; 11; 12; 13; 47; 48; 49; 50; 51; 52; 71]
  let name = "int64"
  let t_to_json_int x =
    if x < Int64.of_int max_int then
      `Int (Int64.to_int x)
    else
      `String (Int64.to_string x)
end

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

module TestMake (Simple : Impl) = struct

let rec json_of_cbor : Simple.t -> Yojson.Basic.t = function
| (`Null | `Bool _ | `Float _ as x) -> x
| `Int x -> Simple.t_to_json_int x
| `Undefined | `Simple _ -> `Null
| `Bytes x -> `String x
| `Text x -> `String x
| `Array x -> `List (List.map json_of_cbor x)
| `Map x -> `Assoc (List.map (fun (k,v) ->
  match k with
  | `Text s -> s, json_of_cbor v
  | _ -> fail "json_of_cbor: expected string key") x)

let run () =
  match List.tl @@ Array.to_list Sys.argv with
  | [] ->
    eprintfn "E: no test file given";
    exit 2
  | files ->
    eprintfn "I: running %s tests from %s" Simple.name (String.concat ", " files);
    let tests =
      List.fold_left
        (fun all_tests file -> all_tests @ read file)
        []
        files
    in
    eprintfn "I: total %s tests = %d" Simple.name(List.length tests);
    tests |> List.fold_left (fun (successes, warnings, failures) test ->
      let nr = successes + warnings + failures in
      let report success s =
        let ignore = List.mem nr Simple.expected_failures in
        let status, tally =
          match success, ignore with
          | true, _ -> "I:", (successes + 1, warnings, failures)
          | false, true -> "W:", (successes, warnings + 1, failures)
          | false, false -> "E:", (successes, warnings, failures +1)
        in
        eprintfn "%s %s test %d: %s" status Simple.name nr s;
        tally
      in
      let test () =
        let cbor = Simple.decode test.cbor in
        let diag = Simple.to_diagnostic cbor in
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
        eprintfn "I: %s tests finished. tests ok = %d failed = %d ignored = %d"
          Simple.name successes failures warnings
    )

end

module Test = TestMake(Impl)
module Test64 = TestMake(Impl64)

let () =
  Test.run ();
  Test64.run ()
