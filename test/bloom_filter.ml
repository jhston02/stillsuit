open Stillsuit

module String_filter = Bloom_filter.MakeSeeded(String)

let bloom_filter__basic_get_set__always_has_value = 
  QCheck2.Test.make ~count:1000 ~name:"Bloom_filter basic get set always has value" QCheck2.Gen.char 
    (fun v -> 
        let filter = Bloom_filter.create 1000 in
        Bloom_filter.add filter v;
        Bloom_filter.is_present filter v)


let bloom_filter__get_non_present_value__false_positive_within_acceptable_rate =
  QCheck2.Test.make ~count:100 ~name:"Bloom_filter get non existent value returns false" (QCheck2.Gen.pair  QCheck2.Gen.neg_int QCheck2.Gen.nat)
    (fun (v,c) ->
      let filter = Bloom_filter.create 1000 in
      Bloom_filter.add filter v;
      Bloom_filter.is_present filter c = false)


let string_filter__basic_get_set__always_has_value = 
  QCheck2.Test.make ~count:1000 ~name:"String_filter basic get set always has value" QCheck2.Gen.string 
    (fun v -> 
        let filter = String_filter.create 1000 in
        String_filter.add filter v;
        String_filter.is_present filter v)

let unique_string_pair_generate =
  let open QCheck2.Gen in
  let+ val1 = string
  and+ val2 = string in
  if val1 = val2 then
  (String.cat val1 "1", val2) 
  else
  (val1, val2)

let string_filter__get_non_present_value__false_positive_within_acceptable_rate =
  QCheck2.Test.make ~count:1000 ~name:"String_filter get non existent value returns false" unique_string_pair_generate
    (fun (v,c) ->
      print_endline v;
      print_endline c;
      let filter = String_filter.create 1000 in
      String_filter.add filter v;
      String_filter.is_present filter c = false)


let bloom_filter__get_length__returns_item_count =
  let bf = Bloom_filter.create 1000 in
  let counter = ref 0 in
  QCheck2.Test.make ~count:1000 ~name: "Bloom_filter length returns item count" QCheck2.Gen.string
  (fun v ->
    counter := !counter + 1;
    Bloom_filter.add bf v; 
    Bloom_filter.length bf = !counter)

let generate_bloom_filter = 
  let open QCheck2.Gen in
  string |> list |> map 
    (fun values -> 
      let f = String_filter.create 1000 in
      let filter = List.fold_left (fun acc v -> String_filter.add acc v; acc) f values in
      (values, filter))


let bloom_filter__copy__copies_are_equal =
  QCheck2.Test.make ~count:100 ~name:"Bloom filter copy creates copy which is equal" generate_bloom_filter 
    (fun (_, f) -> 
      let copy = String_filter.copy f in
      String_filter.equals copy f)


let bloom_filter__serialize_deserialize__is_equal =
  QCheck2.Test.make ~count:1000 ~name:"Round trip bloom filter serialization" generate_bloom_filter
    (fun (_, f) ->
      let s = String_filter.to_bytes f in
      let c = String_filter.from_bytes s in
      String_filter.equals f c)




let property_tests =
  ( "Property",
    List.map
      (QCheck_alcotest.to_alcotest ~verbose:true)
      [ bloom_filter__basic_get_set__always_has_value ; 
        bloom_filter__copy__copies_are_equal;
        bloom_filter__get_length__returns_item_count;
        bloom_filter__get_non_present_value__false_positive_within_acceptable_rate;
        bloom_filter__serialize_deserialize__is_equal;
        string_filter__basic_get_set__always_has_value;
        string_filter__get_non_present_value__false_positive_within_acceptable_rate] )