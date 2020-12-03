let prevedi_vsebino_datoteke_v_seznam vsebina =
    (* Str.split (vsebina '\n') *)
    String.split_on_char '\n' vsebina  (* char je v enojnih narekovajih *)


let prevedi_seznam_stringov_v_seznam_intov sez = 
    List.map int_of_string sez


let pridobi_seznam vsebina_datoteke =
    prevedi_seznam_stringov_v_seznam_intov (
        prevedi_vsebino_datoteke_v_seznam vsebina_datoteke
    )


(*
Testiranje obdelovanja vhodne datoteke:

let rec izpisi_seznam_stevil = (function
    | [] -> ()
    | x :: ostanek -> (
        print_int x; print_string " "; izpisi_seznam_stevil ostanek;
    )
)

let nov_sez = prevedi_seznam_stringov_v_seznam_intov (
    prevedi_vsebino_datoteke_v_seznam "123\n456\n789"
);;

let () = izpisi_seznam_stevil nov_sez;;
*)


let rec vsota (sez : int list) (meja : int) (stevilo_clenov : int) : int list = (
    let rec zapelji_se_po_seznamu (sez' : int list) : int list = (
        match sez' with
        | [] -> [-1]
        | x :: ostanek -> (
            match vsota ostanek (meja - x) (stevilo_clenov - 1) with
            | -1 :: [] -> zapelji_se_po_seznamu ostanek
            | seznam -> x :: seznam
        )
    )
    in
    match meja with
    | 0 ->  (
        match stevilo_clenov with
        | 0 -> []
        | _ -> [-1]
    )
    | presezena_vsota when presezena_vsota < 0 -> [-1]
    | _ -> (zapelji_se_po_seznamu sez)
)
(* -1 označuje napako, npr. "vsota je presegla mejo" ali pa "prišli smo do konca seznama" *)


(* 
Testni seznam:

let neki = vsota [1721; 979; 366; 299; 675; 1456] 2020 3;;
let zmnozek = List.fold_left ( * ) 1 neki;;
print_int zmnozek;;
*)


let zmnozi_seznam (sez : int list) : string =
    string_of_int (
        List.fold_left ( * ) 1 (
            sez
        )
    )  (* še prevedli bomo produkt v string *)


let meja = 2020  (* toliko mora znašati vsota členov *)


let resi_nalogo (vsebina_datoteke : string) (stevilo_clenov : int) : string = 
    zmnozi_seznam (
        vsota (
            pridobi_seznam vsebina_datoteke
        ) meja stevilo_clenov  (* funkcija "vsota" sprejme seznam števil, mejo, in pa število členov *)
    )


let naloga1 (vsebina_datoteke : string) =
    resi_nalogo vsebina_datoteke 2


let naloga2 vsebina_datoteke =
    resi_nalogo vsebina_datoteke 3


let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "day_1.in" in
    (* print_string vsebina_datoteke; *)
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_1_1.out" odgovor1;
    izpisi_datoteko "day_1_2.out" odgovor2
