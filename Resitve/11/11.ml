let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
    String.split_on_char '\n' vsebina


let iz_stringa_v_seznam_charov (s : string) : char list =
    let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
        match indeks with
        | a when a = String.length s' -> []
        | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
    in
    pomozna_funkcija s 0


let sirina_tabele (tabela : char list list) : int =
    (* vzamemo prvo vrstico tabele *)
    (* String.length (List.nth tabela 0) *)
    List.length (List.nth tabela 0)


let visina_tabele (tabela : char list list) : int =
    List.length tabela


let preberi_polje_tabele (tabela : char list list) (x : int) (y : int) : char =
    let vrstica = List.nth tabela y
    in
    (* String.get vrstica x *)
    List.nth vrstica x


let polje_je_zasedeno (tabela : char list list) (x : int) (y : int) : int =
    match x, y with
    | x', _ when x' < 0 || x' >= sirina_tabele tabela -> 0
    | _, y' when y' < 0 || y' >= visina_tabele tabela -> 0
    | x', y' -> if (preberi_polje_tabele tabela x y) = '#' then 1 else 0


let polje_je_zasedeno_2 (tabela : char list list) (x : int) (y : int) : char =  (* za drugi del naloge *)
    match x, y with
    | x', _ when x' < 0 || x' >= sirina_tabele tabela -> 'L'
    | _, y' when y' < 0 || y' >= visina_tabele tabela -> 'L'
    | x', y' -> preberi_polje_tabele tabela x y


let rec pomnozi_vrsto_s_clenom vrsta clen = (* iz [x - 1, x, x + 1] * y naredi [(x - 1, y), (x, y), (x + 1, y)] *)
    match vrsta with
    | [] -> []
    | x :: ostanek_x -> (x, clen) :: pomnozi_vrsto_s_clenom ostanek_x clen


let rec ustvari_produkt_seznamov vrsta stolpec =
    match vrsta, stolpec with
    | sez_x, [] -> []
    | sez_x, y :: ostanek_y -> (pomnozi_vrsto_s_clenom sez_x y) :: ustvari_produkt_seznamov sez_x ostanek_y


(*
let curryiranje (f : ('a * 'b) -> 'c) =
    fun a b -> f (a, b)
*)


let obratno_curryiranje (f : 'a -> 'b -> 'c) =
    fun (a, b) -> f a b


let vrni_stevilo_zasedenih_mest_okoli_polja (tabela : char list list) (x : int) (y : int) : int =
    let vrsta = [x - 1; x; x + 1]
    in
    let stolpec = [y - 1; y; y + 1]
    in
    (* [[(x - 1, y - 1), (x, y - 1), ...], [], []]) *)
    let matrika_koordinat = ustvari_produkt_seznamov vrsta stolpec
    in
    (* let () = izpisi_seznam_seznamov_stevil' matrika_koordinat in *)
    let matrika = List.map (fun vrsta -> (List.map (obratno_curryiranje (polje_je_zasedeno tabela)) vrsta)) matrika_koordinat  (* matrika enic in ničel *)
    in
    let stevilo_zasedenih_v_matriki = List.fold_left (+) 0 (List.map (fun vrstica -> List.fold_left (+) 0 vrstica) matrika)
    in
    let x_y_je_zaseden = polje_je_zasedeno tabela x y
    in
    stevilo_zasedenih_v_matriki - x_y_je_zaseden


let rec v_tej_smeri_se_vidi_sedez (tabela : char list list) (x : int) (y : int) (x' : int) (y' : int) : int =
    match polje_je_zasedeno_2 tabela x y with
    | '#' -> 1
    | 'L' -> 0
    | '.' -> v_tej_smeri_se_vidi_sedez tabela (x + x') (y + y') x' y'
    | _ -> failwith "Neveljaven znak!"


let osnova = [(1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1); (0, -1); (1, -1)]  (* vse možne smeri *)


let vrne_stevilo_vidnih_zasedenih_sedezev (tabela : char list list) (x : int) (y : int) : int =
    let funkcija_glede_na_smer = v_tej_smeri_se_vidi_sedez tabela x y
    in
    let seznam_enic_in_nicel = List.map (funkcija_glede_na_smer |> obratno_curryiranje) osnova
    in
    List.fold_left (+) 0 seznam_enic_in_nicel


let nov_char_v_polju (nacin_stetja_sosedov : char list list -> int -> int -> int) (meja : int) (tabela : char list list) (x : int) (y : int) : char =
    let prejsni_char = preberi_polje_tabele tabela x y
    in
    match prejsni_char with
    | '.' -> '.'
    | _ -> (
        let stevilo_sosedov = nacin_stetja_sosedov tabela x y
        in
        match prejsni_char with
            | 'L' -> (
                if stevilo_sosedov = 0 then
                    '#'
                else
                    'L'
            )
            | '#' -> (
                if stevilo_sosedov >= meja then
                    'L'
                else
                    '#'
            )
            | _ -> failwith "Neustrezen znak!"
    )


let naredi_tabelo_indeksov (sirina : int) (visina : int) =
    let rec pomozna_funkcija (sirina' : int) (visina' : int) (zacetni_indeks : int) : (int * int) list list = 
        (* [[(0, 0), (1, 0), ...], [], ...] *)
        let vrstica (dolzina : int) y =
            let rec pomozna_funkcija' (dolzina' : int) y (zacetni_indeks' : int) = 
                match dolzina' with
                | 0 -> []
                | indeks -> (zacetni_indeks', y) :: pomozna_funkcija' (dolzina' - 1) y (zacetni_indeks' + 1)
            in
            pomozna_funkcija' dolzina y 0
        in
        match visina' with
        | 0 -> []
        | indeks -> (vrstica sirina' zacetni_indeks) :: pomozna_funkcija sirina' (visina' - 1) (zacetni_indeks + 1)
    in
    pomozna_funkcija sirina visina 0


let nova_matrika (nacin_stetja_sosedov : char list list -> int -> int -> int) (meja : int) (tabela : char list list) (sirina : int) (visina : int) tabela_indeksov : char list list =
    List.map (fun vrsta -> List.map (obratno_curryiranje (nov_char_v_polju nacin_stetja_sosedov meja tabela)) vrsta) tabela_indeksov


let vrni_matriko_ko_se_postopek_konca (nacin_stetja_sosedov : char list list -> int -> int -> int) (meja : int) (tabela : char list list) : char list list =
    let sirina = sirina_tabele tabela
    in
    let visina = visina_tabele tabela
    in
    let tabela_indeksov = naredi_tabelo_indeksov sirina visina
    in
    let rec pomozna (nacin_stetja_sosedov : char list list -> int -> int -> int) (meja : int) (tabela' : char list list) : char list list =
        let nova = nova_matrika nacin_stetja_sosedov meja tabela' sirina visina tabela_indeksov
        in
        match nova with
        | matrika when matrika = tabela' -> tabela'
        | _ -> pomozna nacin_stetja_sosedov meja nova
    in
    pomozna nacin_stetja_sosedov meja tabela


let prestej_vse_zasedene (tabela : char list list) : int =
    List.fold_left (+) 0 (List.map (fun vrsta -> List.fold_left (+) 0 (List.map (fun clen -> if clen = '#' then 1 else 0) vrsta)) tabela)


let naloga1 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> (List.map iz_stringa_v_seznam_charov) |> ((vrni_matriko_ko_se_postopek_konca vrni_stevilo_zasedenih_mest_okoli_polja) 4) |> prestej_vse_zasedene |> string_of_int

(*
let naloga2 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> (List.map iz_stringa_v_seznam_charov) |> (vrni_matriko_ko_se_postopek_konca vrne_stevilo_vidnih_zasedenih_sedezev 5) |> prestej_vse_zasedene |> string_of_int
*)

let () = Sys.chdir "Resitve"
let () = Sys.chdir "11"


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
    let vsebina_datoteke = preberi_datoteko "day_11.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    (* and odgovor2 = naloga2 vsebina_datoteke *)
    in
    izpisi_datoteko "day_11_1.out" odgovor1 (* ;
    izpisi_datoteko "day_11_2.out" odgovor2*) 