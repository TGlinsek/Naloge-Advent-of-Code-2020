let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
    String.split_on_char '\n' vsebina


let prevedi_seznam (sez : string list) : int list =
    List.map int_of_string sez


let vsaj_dva_elementa_sta_enaka (sez : 'a list) : bool =
    let rec pomozna_funkcija (originalen_sez : 'a list) (sez_prejsnjih_elementov : 'a list) : bool =
        match originalen_sez with
        | x :: ostanek -> (List.mem x sez_prejsnjih_elementov) || pomozna_funkcija ostanek (x :: sez_prejsnjih_elementov)
        | [] -> false
    in
    pomozna_funkcija sez []


(*
let rec izpisi_seznam_stevil = (function
    | [] -> ()
    | x :: ostanek -> (
        print_int x; print_string " "; izpisi_seznam_stevil ostanek;
    )
)

let rec izpisi_seznam_seznamov_stevil = (function
    | [] -> ()
    | sez :: ostanek -> (
        izpisi_seznam_stevil sez; print_string "\n;;;;;"; izpisi_seznam_seznamov_stevil ostanek;
    )
)
*)


let ima_ustrezna_sestevanca ((sez, vsota) : ('a list * int)) : bool =
    if vsaj_dva_elementa_sta_enaka sez then
        failwith "Več enakih elementov!"
    else        
        let predelan_seznam = List.map (fun x -> Stdlib.min x (vsota - x)) sez  (* lahko bi tudi dali max, vseeno je *)
        in
        vsaj_dva_elementa_sta_enaka predelan_seznam


let rec vrni_prvih_n_elementov_seznama (sez' : 'a list) (n : int) : ('a list * int) =
    match sez' with
    | x :: ostanek when n = 0 -> ([], x)
    | [] -> failwith "Ni dovolj elementov v seznamu."
    | y :: ostanek -> (
        let seznam, st = vrni_prvih_n_elementov_seznama ostanek (n - 1)
        in (y :: seznam, st)
    )


let rec najdi_stevilo (sez : int list) : int =
    let delni_seznam = vrni_prvih_n_elementov_seznama sez 25
    in
    if ima_ustrezna_sestevanca delni_seznam
        then najdi_stevilo (List.tl sez)
    else 
        snd delni_seznam


(* ------- Še druga naloga ------- *)


let rec prvi_prevelik_indeks (sez : int list) (meja : int) : int =
    match sez with
    | [] -> 0
    | x :: ostanek -> (
        if x < meja then 
            1 + prvi_prevelik_indeks ostanek meja 
        else 
            0
    )


let rec razdeli_sez_na_dva_dela (sez : int list) (indeks_locevanja : int) : (int list * int list) =  (* indeks je začetni indeks drugega seznama *)
    if indeks_locevanja <= 0 then 
        ([], sez) 
    else
        match sez with
        | x :: ostanek -> (
            let prvi, drugi = razdeli_sez_na_dva_dela ostanek (indeks_locevanja - 1)
            in
            (x :: prvi, drugi)
        )
        | [] -> ([], [])


let rec ustvari_podsezname (sez : int list) (meja : int) : int list list =
    let indeks = prvi_prevelik_indeks sez meja
    in
    match sez with
    | [] -> []
    | x :: ostanek when indeks = 0 -> (
        ustvari_podsezname ostanek meja
    )
    | x :: ostanek -> (
        let prviDel, drugiDel = razdeli_sez_na_dva_dela sez indeks
        in
        prviDel :: (ustvari_podsezname drugiDel meja)
    )


let rec najdi_vecterico_iz_leve (sez : int list) (meja : int) (zadnji_indeks : int) : int =  (* vrne zadnji indeks *)
    if meja < 0 then 
        -1000 
    else 
        match sez with
        | x :: ostanek -> (
            if meja - x = 0 then zadnji_indeks else
            najdi_vecterico_iz_leve ostanek (meja - x) (zadnji_indeks + 1)
        )
        | [] -> -1000


let rec najdi_vecterico (sez : int list) (meja : int) (prvi_indeks : int) : (int * int) =  (* vrne (0, -1000), če nič ni bilo najdeno *)
    match sez with
    | x :: ostanek -> (
        let zadnji_indeks = (najdi_vecterico_iz_leve ostanek (meja - x) prvi_indeks + 1)
        in
        if zadnji_indeks <= -1 then
            najdi_vecterico ostanek meja (prvi_indeks + 1)
        else
            (prvi_indeks, zadnji_indeks)
    )
    | [] -> (0, -1000)
(* par indeksov, ki ga funkcija najdi_vecterico vrne, je oblike (n, m), kjer sta n-ti in m-ti člen še vključena v podmnožico *)


let rezina (sez : 'a list) (n : int) (m : int) : 'a list =  (* to niso pythonovske rezine, so pa skoraj *)
    if n > m then 
        failwith "Napačni indeksi!" 
    else
        snd (
            razdeli_sez_na_dva_dela (
                fst (
                    razdeli_sez_na_dva_dela sez (m + 1)
                )
            ) n
        )


let rec min_v_seznamu (sez : 'a list) : int =
    match sez with
    | x :: [] -> x
    | x :: ostanek -> Stdlib.min x (min_v_seznamu ostanek)
    | [] -> failwith "Prazen seznam!"


let rec maks_v_seznamu (sez : 'a list) : int =
    match sez with
    | x :: [] -> x
    | x :: ostanek -> Stdlib.max x (maks_v_seznamu ostanek)
    | [] -> failwith "Prazen seznam!"


let vrni_min_maks (sez : int list) (meja : int) : (int * int) =
    let a, b = najdi_vecterico sez meja 0
    in
    if b <= -1 then 
        (-1000, -1000) 
    else
        let izsek = rezina sez a b
        in
        let maks = maks_v_seznamu izsek
        in
        let min = min_v_seznamu izsek
        in
        min, maks


let encryption_weakness (sez : int list) (meja : int) : int =
    let min, maks = vrni_min_maks sez meja
    in
    let vsota = min + maks
    in
    if vsota < 0 then 
        0 
    else 
        vsota


let rec preisci (podseznami : int list list) (meja : int) : int =
    match podseznami with
    | sez' :: ostanek -> (
        let vrednost = encryption_weakness sez' meja
        in
        if vrednost = 0 then 
            preisci ostanek meja 
        else 
            vrednost
    )
    | [] -> failwith "Ni bilo najdenega zadetka!"


let razdeli_in_preisci (sez : int list) : int =
    let meja = najdi_stevilo sez  (* izvedemo spet prvo nalogo, saj rabimo rezultat iz prejšnje naloge *)
    in
    let podseznami = ustvari_podsezname sez meja
    in
    preisci podseznami meja


let naloga1 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> prevedi_seznam |> najdi_stevilo |> string_of_int


let naloga2 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> prevedi_seznam |> razdeli_in_preisci |> string_of_int


let () = Sys.chdir "Resitve"
let () = Sys.chdir "9"


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
    let vsebina_datoteke = preberi_datoteko "day_9.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke 
    in
    izpisi_datoteko "day_9_1.out" odgovor1;
    izpisi_datoteko "day_9_2.out" odgovor2 
