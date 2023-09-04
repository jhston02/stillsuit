type 'a t =  { hashes: ('a -> int) list; filter: Bitv.t;} 

let calc_filter_size count prob =
  let count = Float.of_int count in
  -.(count *. log prob) /. (log 2. ** 2.)

let get_hash_count count prob =
  let count = Float.of_int count in
  count /. prob *. log 2.

let get_hashes hash_count (seeded_hashes:(int -> 'a -> int)) =
  let range =
    List.init hash_count (fun x -> x)
  in
  List.map (fun x -> seeded_hashes x) range

let create ?(fp_prob=0.01) count =
  let size = Int.of_float (calc_filter_size count fp_prob) in
  let hash_count = Int.of_float (get_hash_count count fp_prob) in
  let functions = (get_hashes hash_count) Hashtbl.seeded_hash in
  { filter = Bitv.create size false; hashes = functions}

let add bloom_filter item =
  List.iter (fun hash ->
    let index = hash item mod (Bitv.length bloom_filter.filter) in
    Bitv.set bloom_filter.filter index true
  ) bloom_filter.hashes

let is_present bloom_filter item =
  List.for_all (fun hash ->
    let index = hash item mod (Bitv.length bloom_filter.filter) in
    Bitv.get bloom_filter.filter index
  ) bloom_filter.hashes

let clear bloom_filter =
  Bitv.fill bloom_filter.filter 0 (Bitv.length bloom_filter.filter) false

let length bloom_filter =
  Bitv.length bloom_filter.filter

let copy bloom_filter =
  { bloom_filter with filter = Bitv.copy bloom_filter.filter }

let accumulate bloom_filter1 bloom_filter2 =
  {bloom_filter1 with filter = Bitv.bw_or bloom_filter1.filter bloom_filter2.filter}

let contains bloom_filter1 bloom_filter2 =
  Bitv.bw_and bloom_filter1.filter bloom_filter2.filter = bloom_filter1.filter

let to_bytes bloom_filter =
  let filter_bytes = Bitv.to_bytes bloom_filter.filter in
  let filter_bytes_length = Bytes.length filter_bytes in
  let number_of_hashes = List.length bloom_filter.hashes in
  let length = 8 + 8 + filter_bytes_length in
  let bytes = Bytes.create length in
  Bytes.set_int64_be bytes 0 (Int64.of_int(filter_bytes_length));
  Bytes.set_int64_be bytes 8 (Int64.of_int(number_of_hashes));
  Bytes.blit filter_bytes 0 bytes 16 filter_bytes_length;
  bytes

let from_bytes bytes =  
  let filter_bytes_length = Int64.to_int (Bytes.get_int64_be bytes 0) in
  let number_of_hashes = Int64.to_int (Bytes.get_int64_be bytes 8) in
  let filter_bytes = Bytes.sub bytes 16 filter_bytes_length in
  let filter = Bitv.of_bytes filter_bytes in
  let hashes = (get_hashes number_of_hashes Hashtbl.seeded_hash) in
  { hashes = hashes; filter = filter }


module type Seeded_Hashable = sig
  type t
  val hash: int -> t -> int
end

module type S = sig
  type value
  type t
  val add: t -> value -> unit
  val is_present: t -> value -> bool
  val create : ?fp_prob:float -> int -> t
  val clear: t -> unit
  val length: t -> int
  val accumulate: t -> t -> t
  val contains: t -> t -> bool
  val copy: t -> t
  val to_bytes: t -> bytes
  val from_bytes: bytes -> t
 end

 module MakeSeeded (H : Seeded_Hashable) : (S with type value = H.t) = struct
  type value = H.t  
  type  t =  { hashes: (value -> int) list; filter: Bitv.t;} 

   let create ?(fp_prob=0.01) count =
    let size = Int.of_float (calc_filter_size count fp_prob) in
    let hash_count = Int.of_float (get_hash_count count fp_prob) in
    let functions = (get_hashes hash_count) Hashtbl.seeded_hash in
    { filter = Bitv.create size false; hashes = functions}
  
  let add bloom_filter item =
    List.iter (fun hash ->
      let index = hash item mod (Bitv.length bloom_filter.filter) in
      Bitv.set bloom_filter.filter index true
    ) bloom_filter.hashes
  
  let is_present bloom_filter item =
    List.for_all (fun hash ->
      let index = hash item mod (Bitv.length bloom_filter.filter) in
      Bitv.get bloom_filter.filter index
    ) bloom_filter.hashes
  
  let clear bloom_filter =
    Bitv.fill bloom_filter.filter 0 (Bitv.length bloom_filter.filter) false
  
  let length bloom_filter =
    Bitv.length bloom_filter.filter
  
  let copy bloom_filter =
    { bloom_filter with filter = Bitv.copy bloom_filter.filter }
  
  let accumulate bloom_filter1 bloom_filter2 =
    {bloom_filter1 with filter = Bitv.bw_or bloom_filter1.filter bloom_filter2.filter}
  
  let contains bloom_filter1 bloom_filter2 =
    Bitv.bw_and bloom_filter1.filter bloom_filter2.filter = bloom_filter1.filter
  
  let to_bytes bloom_filter =
    let filter_bytes = Bitv.to_bytes bloom_filter.filter in
    let filter_bytes_length = Bytes.length filter_bytes in
    let number_of_hashes = List.length bloom_filter.hashes in
    let length = 8 + 8 + filter_bytes_length in
    let bytes = Bytes.create length in
    Bytes.set_int64_be bytes 0 (Int64.of_int(filter_bytes_length));
    Bytes.set_int64_be bytes 8 (Int64.of_int(number_of_hashes));
    Bytes.blit filter_bytes 0 bytes 16 filter_bytes_length;
    bytes
  
  let from_bytes bytes =  
    let filter_bytes_length = Int64.to_int (Bytes.get_int64_be bytes 0) in
    let number_of_hashes = Int64.to_int (Bytes.get_int64_be bytes 8) in
    let filter_bytes = Bytes.sub bytes 16 filter_bytes_length in
    let filter = Bitv.of_bytes filter_bytes in
    let hashes = (get_hashes number_of_hashes Hashtbl.seeded_hash) in
    { hashes = hashes; filter = filter }
    
 end