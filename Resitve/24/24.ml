let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list = (
    String.split_on_char '\n' vsebina
)


let iz_stringa_v_seznam_charov (s : string) : char list =
    let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
        match indeks with
        | a when a = String.length s' -> []
        | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
    in
    pomozna_funkcija s 0


(* ---------------------------------------- *)


type smeri =
    | E
    | W
    | NW
    | NE
    | SW
    | SE


let rec pretvori_seznam_znakov_v_seznam_smeri (vrsta : char list) : smeri list = (
    match vrsta with
    | [] -> []
    | 'e' :: xs -> E :: pretvori_seznam_znakov_v_seznam_smeri xs
    | 'w' :: xs -> W :: pretvori_seznam_znakov_v_seznam_smeri xs
    | a :: b :: xs -> (
        (
            match a, b with
            | 'n', 'w' -> NW
            | 'n', 'e' -> NE
            | 's', 'w' -> SW
            | 's', 'e' -> SE
            | _ -> failwith "Neustrezni znaki v seznamu!"
        )
        :: pretvori_seznam_znakov_v_seznam_smeri xs
    )
    | _ -> failwith "Neustrezni znaki v seznamu!"
)


(* ---------------------------------------- *)


type koordinate = {
    x : int;
    y : int
}


let pretvori (nabor : int * int) : koordinate =
    {x = (fst nabor); y = (snd nabor)}


let (++) (a : koordinate) (b : koordinate) : koordinate =
    {x = a.x + b.x; y = a.y + b.y}


(* os SW-NE predstavlja y os (NE poveča y koordinato), os W-E x os (E poveča x koordinato), SE-WN pa je kombinacija obeh *)
let rec smeri_priredi_koordinate (pot : smeri list) : koordinate =
    match pot with
    | smer :: xs -> (
        pretvori (
            match smer with
            | E -> (1, 0)
            | W -> (-1, 0)
            | NW -> (-1, 1)
            | NE -> (0, 1)
            | SW -> (0, -1)
            | SE -> (1, -1)
        )
        ++ smeri_priredi_koordinate xs
    )
    | [] -> pretvori (0, 0)


(* ---------------------------------------- *)


let prestej_crne_ploscice_v_seznamu_vrstic (seznam : 'a list) : int =  (* če se element seznama ponovi lihokrat, se šteje v skupni seštevek, če sodokrat, pa ne *) 
    let ali_element_v_seznamu (seznam' : 'a list) (el : 'a) : (('a list) option) =  (* le za sezname, v katerih se el ne pojavi več kot enkrat *)
        (* če element je v seznamu, se vrne isti seznam, a brez tistega elementa *)
        let rec pomozna (seznam'' : 'a list) : ('a list * bool) =
            match seznam'' with
            | [] -> ([], false)
            | x :: xs -> (
                let sez, status = pomozna xs
                in
                if x = el then (
                    if status = true then
                        failwith "Seznam ne bi smel imeti dva enaka elementa!"
                    else
                        (sez, true)
                )
                else (x :: sez, status)
            )
        in
        let nov_sez, odgovor = pomozna seznam'
        in
        if odgovor = false then
            None
        else
            Some nov_sez
    in
    let prestej (sez' : 'a list) : int =
        let rec pomozna'' (sez'' : 'a list) : ('a list * int) =
            match sez'' with
            | [] -> [], 0
            | x :: xs -> (
                let delni_seznam, stevec = pomozna'' xs
                in
                if stevec < 0 then
                    failwith "Stevec mora vedno biti nenegativen!"
                else
                    let obdelan_delni_seznam = ali_element_v_seznamu delni_seznam x
                    in
                    match obdelan_delni_seznam with
                    | Some s -> s, stevec - 1
                    | None -> x :: delni_seznam, stevec + 1
            )
        in
        snd (pomozna'' sez')
    in
    prestej seznam


(* ---------------------------------------- *)


let naloga1 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> List.map iz_stringa_v_seznam_charov |> List.map pretvori_seznam_znakov_v_seznam_smeri |> List.map smeri_priredi_koordinate |> prestej_crne_ploscice_v_seznamu_vrstic |> string_of_int


(* ---------------------------------------- *)


(* S slovarji te funkcije ne bi potrebovali *)
let rec koliko_ponovitev_elementa_v_seznamu (seznam : 'a list) (el : 'a) : int =  (* če se element le enkrat pojavi, se to šteje kot ena ponovitev *)
    match seznam with
    | x :: xs -> (
        (if x = el then 1 else 0)
        + koliko_ponovitev_elementa_v_seznamu xs el
    )
    | [] -> 0


let e = pretvori (1, 0)
let w = pretvori (-1, 0)
let nw = pretvori (-1, 1)
let ne = pretvori (0, 1)
let sw = pretvori (0, -1)
let se = pretvori (1, -1)


let vrni_sosede_tocke (tocka : koordinate) : koordinate list =
    let tocka' = pretvori (tocka.x, tocka.y)
    in
    [
        tocka' ++ e;
        tocka' ++ w;
        tocka' ++ nw;
        tocka' ++ ne;
        tocka' ++ sw;
        tocka' ++ se
    ]


(* V pythonu bi to lahko naredili s slovarji. Ključi so koordinate, vrednosti pa število pojavitev v seznamu_sosedov *)
let rec seznam_sosedov (seznam_crnih : koordinate list) : koordinate list =  (* seznam_crnih vsebuje samo unikatne elemente *)
    match seznam_crnih with
    | tocka :: xs -> (
        (vrni_sosede_tocke tocka) @ (seznam_sosedov xs)  (* tukaj hočemo s ponovitvami *)
    )
    | [] -> []


(* Vsi v seznamu_sosedov, ki niso v "seznam"-u, so beli *)
(* 
Pogoj iz naloge lahko povemo tako:
Izberi vse, ki so "sosede natanko dveh črnih" ali "soseda ene do dveh črnih in so črne". Natanko te so črne v naslednji rundi.
*)
let primerjaj (seznam : koordinate list) (seznam_sosedov' : koordinate list) : koordinate list =  (* vrne vse, ki bodo črni v naslednji rundi *)
    let rec pomozna_funkcija (sez_sosedov : koordinate list) : (koordinate list * koordinate list) =
        match sez_sosedov with
        | x :: xs -> (
            let stevilo_iksov_v_sosedih = koliko_ponovitev_elementa_v_seznamu seznam_sosedov' x
            in
            let iks_v_seznamu = (koliko_ponovitev_elementa_v_seznamu seznam x) > 0
            in
            let ostanek = pomozna_funkcija xs
            in
            if (koliko_ponovitev_elementa_v_seznamu (snd ostanek) x) > 0 then ostanek
            else
            (
                if (stevilo_iksov_v_sosedih = 2) || (stevilo_iksov_v_sosedih = 1 && iks_v_seznamu) then 
                    x :: (fst ostanek), x :: (snd ostanek)
                else
                    fst ostanek, x :: (snd ostanek)
            )
        )
        | [] -> [], []
    in
    fst (pomozna_funkcija seznam_sosedov')


(* ---------------------------------------- *)


let rec odstrani_dvojnike_iz_seznama (seznam : 'a list) : 'a list =
    match seznam with
    | x :: xs -> (
        let iks_v_seznamu = (koliko_ponovitev_elementa_v_seznamu xs x) > 0
        in
        (if not iks_v_seznamu then [x] else []) 
        @ (odstrani_dvojnike_iz_seznama xs)
    )
    | [] -> []


let transformacija (trenutni_seznam : koordinate list) : koordinate list =  (* transformacija v enem dnevu *)
    (primerjaj trenutni_seznam (seznam_sosedov trenutni_seznam))  (* na koncu morajo biti le enojni *)


(* ---------------------------------------- *)


let rec iz_seznama_odstrani_vse_pojavitve_elementa (seznam : 'a list) (element : 'a) : 'a list =
    match seznam with
    | x :: xs -> (
        (
            if x = element then [] else [x]
        ) 
        @ iz_seznama_odstrani_vse_pojavitve_elementa xs element
    )
    | [] -> []


let odstrani_sode (seznam : 'a list) : 'a list = (* odstrani vse pojavitve elementov, ki se pojavijo sodokrat *)
    let rec pomozna'' (sez : 'a list) : 'a list =
        match sez with
        | x :: xs -> (
            let iksov_je_sodo = ((koliko_ponovitev_elementa_v_seznamu seznam x) mod 2) = 0  (* oz. (koliko_ponovitev_elementa_v_seznamu xs x) mod 2 = 1 *)
            in
            if iksov_je_sodo then 
                pomozna'' (iz_seznama_odstrani_vse_pojavitve_elementa xs x)  (* lahko damo tudi sez namesto xs v tem primeru *)
            else 
                x :: (pomozna'' xs)
        )
        | [] -> []
    in
    pomozna'' seznam


let rec ponovi_postopek (funkcija : 'a -> 'a) (n : int) (zacetno_stanje : 'a) : 'a =
    if n = 0 then
        zacetno_stanje
    else
        ponovi_postopek funkcija (n - 1) (funkcija zacetno_stanje)


(* ---------------------------------------- *)


let naloga2 (vsebina : string) : string =  (* Dela na testnih primerih, ampak že tam traja dolgo *)
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> List.map iz_stringa_v_seznam_charov |> List.map pretvori_seznam_znakov_v_seznam_smeri |> List.map smeri_priredi_koordinate |> odstrani_sode |> ponovi_postopek transformacija 100 |> List.length |> string_of_int


(* ---------------------------------------- *)


let () = Sys.chdir "Resitve"
let () = Sys.chdir "24"


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
    let vsebina_datoteke = preberi_datoteko "day_24.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    (* and odgovor2 = naloga2 vsebina_datoteke *)
    in
    izpisi_datoteko "day_24_1.out" odgovor1;
    (* izpisi_datoteko "day_24_2.out" odgovor2 *)