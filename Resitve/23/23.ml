let rec zasuci_seznam_za (seznam : 'a list) (mesta : int) : 'a list =  (* ta prvi element gre na konec *)
    match mesta with
    | 0 -> seznam
    | a when a > 0 -> (
        match seznam with
        | [] -> []
        | x :: ostanek -> zasuci_seznam_za (ostanek @ [x]) (mesta - 1)
    )
    | _ -> failwith "Le nenegativni argumenti so dov"

let vrni_vecjega_glede_na_primerjalno_funkcijo (f : 'a -> int) (a, a' : 'a * 'b) (b, b' : 'a * 'b) : ('a * 'b) =
    if f a >= f b then
        (a, a')
    else
        (b, b')

let minimum_parov (a : int * int) (b : int * int) : (int * int) =  (* glede na prvo komponento v paru *)
    vrni_vecjega_glede_na_primerjalno_funkcijo (fun x -> -x) a b

let maksimum_parov (a : int * int) (b : int * int) : (int * int) =  (* glede na prvo komponento v paru *)
    vrni_vecjega_glede_na_primerjalno_funkcijo (fun x -> x) a b

let rec vrni_neposredno_manjsega_od_stevila (seznam : int list) (stevilo : int) : (int * int) =  (* če vhodno število najmanjše v seznamu, vrne funkcija največjega *)
    (* levi je vrednost, desni je indeks *)
    match seznam with
    | [] -> failwith "Prazen seznam"
    | x :: [] -> (x, 0)
    | x :: ostanek -> (
        let rezultat = (vrni_neposredno_manjsega_od_stevila ostanek stevilo)
        in
        (* <> pomeni xor *)
        if (x < stevilo) <> (fst rezultat < stevilo) then  (* ni vseeno, ali imamo stroge ali nestroge neenačaje *)
            let min = minimum_parov (x, -1) rezultat
            in
            fst min, (snd min) + 1
        else
            let max = maksimum_parov (x, -1) rezultat
            in
            fst max, (snd max) + 1
    )
(*
let igra (seznam : int list) (st_rund : int) : int list =
    odstrani el
    odstranjen_sez, ostalo
    vrni_indeks_destinacije
    vstavi_seznam_za_destinacijo
    zasuci_seznam_za_eno
    igra nov_seznam
*)
let odstrani_trojico (seznam : int list) : (int list * int list) =  (* levi je trojica, desni preostanek *)
    let f = List.nth seznam
    in
    [f 1; f 2; f 3], (f 0) :: (seznam |> List.tl |> List.tl |> List.tl |> List.tl)

let rec razdeli_na_dva_dela (indeks : int) (sez : 'a list) : ('a list * 'a list) =  (* indeks je začetni indeks drugega seznama *)
    if indeks <= 0 then 
        ([], sez) 
    else
        match sez with
        | x :: ostanek -> (
            let prvi, drugi = razdeli_na_dva_dela (indeks - 1) ostanek
            in
            (x :: prvi, drugi)
        )
        | [] -> ([], [])

let rec igra (seznam : int list) (st_rund : int) : int list =
    if st_rund = 0 then
        seznam
    else
        let trojica, ostanek = odstrani_trojico seznam
        in
        let indeks_destinacije = snd (vrni_neposredno_manjsega_od_stevila ostanek (List.nth ostanek 0))
        in
        let prvi_del, drugi_del = razdeli_na_dva_dela (indeks_destinacije + 1) ostanek
        in
        let nov_sez = prvi_del @ (trojica @ drugi_del)
        in
        igra (zasuci_seznam_za nov_sez 1) (st_rund - 1)

let pretvori_char_v_int (znak : char) : int =
    let vrednost = (int_of_char znak) - 48
    in
    if vrednost < 0 || vrednost > 9 then
        failwith "Tega znaka ne moremo prevesti v int!"
    else
        vrednost

let iz_stringa_v_seznam_intov (s : string) : int list =
    let rec pomozna_funkcija (s' : string) (indeks : int) : int list =
        match indeks with
        | a when a = String.length s' -> []
        | _ -> (pretvori_char_v_int s'.[indeks]) :: (pomozna_funkcija s' (indeks + 1))
    in
    pomozna_funkcija s 0

let rec iz_seznama_intov_v_string (sez : int list) : string =
    match sez with
    | [] -> ""
    | x :: ostanek -> String.concat "" [string_of_int x; iz_seznama_intov_v_string ostanek] 


let stevilo_iger1 = 100

let naloga1 (vsebina : string) : string =
    let dobljem_seznam = stevilo_iger1 |> (vsebina |> iz_stringa_v_seznam_intov |> igra)
    in
    let indeks_enke = snd (vrni_neposredno_manjsega_od_stevila dobljem_seznam 2)
    in
    let zasukan_seznam = zasuci_seznam_za dobljem_seznam indeks_enke
    in
    iz_seznama_intov_v_string (List.tl zasukan_seznam)

let milijon = 1000000

let stevilo_lonckov = milijon

let rec naredi_seznam_od_n_do_m (n : int) (m : int) =  (* in n in m sta v seznamu*)
    match m - n with
    | 0 -> [n]
    | _ when m > n -> n :: naredi_seznam_od_n_do_m (n + 1) m
    | _ -> failwith "Zgornja meja mora biti večja kot spodnja."

let pridobi_seznam_z_milijon_cleni (vhodni_seznam : int list) : int list =  (* no, če spremenimo število lončkov, ni milijon *)
    let zacetni_indeks = List.length vhodni_seznam + 1
    in
    vhodni_seznam @ (naredi_seznam_od_n_do_m zacetni_indeks stevilo_lonckov)

let stevilo_iger2 = 10 * milijon

let naloga2 (vsebina : string) : string =
    let dobljem_seznam = stevilo_iger2 |> (vsebina |> iz_stringa_v_seznam_intov |> pridobi_seznam_z_milijon_cleni |> igra)
    in
    let indeks_enke = snd (vrni_neposredno_manjsega_od_stevila dobljem_seznam 2)
    in
    let vrednost = (
        if indeks_enke < stevilo_lonckov - 2 then
            (List.nth dobljem_seznam (indeks_enke + 1)) * (List.nth dobljem_seznam (indeks_enke + 2))
        else
            let zasukan_seznam = zasuci_seznam_za dobljem_seznam 2
            in
            (List.nth zasukan_seznam (indeks_enke + 1)) * (List.nth zasukan_seznam (indeks_enke + 2))
    )
    in
    string_of_int vrednost

let () = Sys.chdir "Resitve"
let () = Sys.chdir "23"


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
    let vsebina_datoteke = preberi_datoteko "day_23.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    (* and odgovor2 = naloga2 vsebina_datoteke *)
    in
    izpisi_datoteko "day_23_1.out" odgovor1;
    (* izpisi_datoteko "day_23_2.out" odgovor2 *)