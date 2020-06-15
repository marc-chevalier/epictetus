[@@@warning "+A"]

(** Build an ALIGNER given a CONTENT module.
*)
module GenericAligner (T: Aligner.CONTENT) : Aligner.ALIGNER with type contents = T.contents
