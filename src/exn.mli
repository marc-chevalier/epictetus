(** Thrown when a printing error happens. Using the standard entry_point
    [print_table] it shouldn't be possible. However, it is when using other
    functions without care. Please note that this is not the only exception that
    can be raised. For instance Invalid_argument is also possible.
*)
exception PrintError of string list

