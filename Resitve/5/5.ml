let prevedi_vsebino_datoteke_v_seznam vsebina =
    (* Str.split (vsebina '\n') *)
    String.split_on_char '\n' vsebina  (* char je v enojnih narekovajih *)


let iz_stringa_v_seznam_charov (s : string) : char list =
    let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
        match indeks with
        | a when a = String.length s' -> []
        | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
    in
    pomozna_funkcija s 0;;

let spremeni_sifro_v_binarno (s : string) : bool list =
    let pretvori (x : char) : bool = 
        match x with
        | 'F' -> false  (* 0 *)
        | 'B' -> true  (* 1 *)
        | 'L' -> false
        | 'R' -> true
        | a -> failwith "Neveljavna črka."
    in
    let rec pretvori_seznam_charov (sez : char list) : bool list =
        match sez with
        | [] -> []
        | x :: ostanek -> (pretvori x) :: (pretvori_seznam_charov ostanek)

    in
    (pretvori_seznam_charov (iz_stringa_v_seznam_charov s))


let rec pretvori_sifre_v_seznamu (sez : string list) : (bool list) list =
    match sez with
    | [] -> []
    | x :: ostanek -> (spremeni_sifro_v_binarno x) :: (pretvori_sifre_v_seznamu ostanek)


let rec vrni_zadnji_clen_seznama_in_preostanek (sez : 'a list) : ('a list * 'a) =
    match sez with
    | [] -> failwith "Prazen seznam!"
    | x :: [] -> [], x
    | x :: ostanek -> (
        let prvi_del, zadnji_clen = vrni_zadnji_clen_seznama_in_preostanek ostanek
        in
        (x :: prvi_del), zadnji_clen
    )

let rec spremeni_sifro_v_id (s : bool list) : int =
    match s with
    | [] -> 0
    | x :: ostanek -> (
        let prvi_del, zadnji_clen = vrni_zadnji_clen_seznama_in_preostanek (x :: ostanek)
        in
        2 * (spremeni_sifro_v_id prvi_del) + (
            if zadnji_clen = true then
                1
            else
                0
        )
    )

let rec primerjaj_dve_sifri (sifra : bool list) (sifra2 : bool list) : ((bool list) * (bool list)) =
    match sifra, sifra2 with
    | [], [] -> [], []
    | (x :: ostanek), [] -> failwith "Šifri nista istih dolžin."
    | [], (x :: ostanek) -> failwith "Šifri nista istih dolžin."
    | (x :: ostanek), (x' :: ostanek') -> (
        if x = x' then
            let o, o' = primerjaj_dve_sifri ostanek ostanek'
            in (x :: o), (x :: o')
        else (
            if x = true then
                (x' :: ostanek', x :: ostanek)  (* levi je minimum, desni maksimum *)
            else 
                (x :: ostanek, x' :: ostanek')
        )
    )

let min_sifra =
    let rec pomozna_funkcija' dolzina =
        match dolzina with
        | 0 -> []
        | a -> true :: (pomozna_funkcija' (dolzina - 1))
    in
    pomozna_funkcija' 10


let maks_sifra =
    let rec pomozna_funkcija'' dolzina =
        match dolzina with
        | 0 -> []
        | a -> false :: (pomozna_funkcija'' (dolzina - 1))
    in
    pomozna_funkcija'' 10


let rec vrni_min (sez : (bool list) list) : bool list =
    match sez with
    | [] -> min_sifra
    | sifra :: ostanek -> (
        fst (primerjaj_dve_sifri sifra (vrni_min ostanek))
    )


let rec vrni_maks (sez : (bool list) list) : bool list =
    match sez with
    | [] -> maks_sifra
    | sifra :: ostanek -> (
        snd (primerjaj_dve_sifri sifra (vrni_maks ostanek))
    )

let rec vsota (sez : (bool list) list) : int =
    match sez with
    | [] -> 0
    | x :: ostanek -> (spremeni_sifro_v_id x) + (vsota ostanek)


let vsota_aritmeticnega_zaporedja_od_n_do_m (n : int) (m : int) : int =
    ((m + n) * (m - n + 1)) / 2


let naloga1 (vsebina : string) =
    let seznam_sifer = pretvori_sifre_v_seznamu (prevedi_vsebino_datoteke_v_seznam vsebina)
    in
    string_of_int (spremeni_sifro_v_id (vrni_maks seznam_sifer))

let naloga2 (vsebina : string) =
    let seznam_sifer = pretvori_sifre_v_seznamu (prevedi_vsebino_datoteke_v_seznam vsebina)
    in
    let min = spremeni_sifro_v_id (vrni_min seznam_sifer)
    in
    let maks = spremeni_sifro_v_id (vrni_maks seznam_sifer)
    in
    let skupno = vsota seznam_sifer
    in
    string_of_int ((vsota_aritmeticnega_zaporedja_od_n_do_m min maks) - skupno);;


Sys.chdir "Resitve";;
Sys.chdir "5";;

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
    let vsebina_datoteke = preberi_datoteko "day_5.in" in
    (* print_string vsebina_datoteke; *)
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_5_1.out" odgovor1;
    izpisi_datoteko "day_5_2.out" odgovor2
