let rec iz_stringa_v_seznam_charov (s : string) (indeks : int) = 
    match indeks with
    | a when a = String.length s -> []
    | _ -> s.[indeks] :: (iz_stringa_v_seznam_charov s (indeks + 1))


let string_v_char_list (s : string) : char list =
    iz_stringa_v_seznam_charov s 0


(*
let rec izpisi_seznam_stevil = (function
    | [] -> ()
    | x :: ostanek -> (
        print_char x; print_string " "; izpisi_seznam_stevil ostanek;
    )
)
let () = izpisi_seznam_stevil (string_v_char_list "nek string");;
*)


let rec stevilo crka niz =
    match niz with
    | x :: ostanek -> (
        (
            if x = crka then
                1
            else
                0
        )
        +
        stevilo crka ostanek
    )
    | [] -> 0


let rec char_na_podanem_mestu niz indeks =
    match niz with
    | [] -> failwith "Indeks presega dolžino seznama!"
    | x :: ostanek when indeks = 0 -> x
    | x :: ostanek -> (
        char_na_podanem_mestu ostanek (indeks - 1)
    )


let xor (a : bool) (b : bool) =
    (a && (not b)) || ((not a) && b)


type naloge =
    | Prva
    | Druga


let rec prestej (sez : (int * int * char * (char list)) list) (naloga : naloge) : int = (
    match sez with
    | x :: ostanek -> ((prestej ostanek naloga) +
        match naloga with
        | Prva -> (
            let min, max, crka, niz = x
            in
            let st = stevilo crka niz
            in
            (
                if min <= st && st <= max then
                    1
                else
                    0
            )
        )
        | Druga -> (
            let indeks1, indeks2, crka, niz = x
            in
            if xor 
                ((char_na_podanem_mestu niz (indeks1 - 1)) = crka) 
                ((char_na_podanem_mestu niz (indeks2 - 1)) = crka) 
            then
                1
            else
                0
        )
    )
    | [] -> 0
)


let prevedi_vsebino_datoteke_v_seznam vsebina =
    String.split_on_char '\n' vsebina


let razdeli_niz_na_stiri_komponente (niz) : int * int * char * (char list) =
    let sez = (String.split_on_char ' ' niz) 
    in
    let a, b, c = (List.nth sez 0, List.nth sez 1, List.nth sez 2)
    in
    let sez2 = String.split_on_char '-' a
    in
    let x, y = (List.nth sez2 0, List.nth sez2 1)
    in
    (int_of_string x, int_of_string y, (b.[0]), string_v_char_list c);;


(*
let komponente = razdeli_niz_na_stiri_komponente "17-18 d: ddddddddddddddddzn";;  (* izgleda je treba dodati tole dvojno podpičje *)
*)


let delna_funkcija vsebina_datoteke =
    prestej (
        List.map razdeli_niz_na_stiri_komponente (
            prevedi_vsebino_datoteke_v_seznam vsebina_datoteke
        )
    ) 


let resi_nalogi vsebina_datoteke (naloga : naloge) =
    string_of_int (
        delna_funkcija vsebina_datoteke naloga
    );;


let naloga1 (vsebina_datoteke : string) = 
    resi_nalogi vsebina_datoteke Prva


let naloga2 (vsebina_datoteke : string) =
    resi_nalogi vsebina_datoteke Druga;;
(* zakaj ne moremo dati oklepajev okoli tega izraza? *)


Sys.chdir "Naloge-Advent-of-Code-2020";;
Sys.chdir "Resitve";;
Sys.chdir "2";;


(* Funkcija kopirana iz vzorčnega programa na spletni učilnici *)
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
    let vsebina_datoteke = preberi_datoteko "day_2.in" in
    (* print_string vsebina_datoteke; *)
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_2_1.out" odgovor1;
    izpisi_datoteko "day_2_2.out" odgovor2
