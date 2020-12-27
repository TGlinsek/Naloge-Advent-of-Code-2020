let prvotno_subjektno_stevilo = 7


let transformacija (a : int) (subjektno_stevilo : int) : int =
    let b = a * subjektno_stevilo
    in
    b mod 20201227


let pridobi_velikost_zanke (cilj : int) : int =
    let rec pomozna (stevec : int) (zacetek : int) (cilj : int) (meja : int) : int =  (* repna rekurzija: pomnilnik lahko pozabi na prejšnje klice funkcije, ker ne vplivajo na rezultat *)
        if meja = 0 then
            failwith "Infinite loop"
        else
        if zacetek = cilj then
            stevec
        else
            pomozna (stevec + 1) (transformacija zacetek prvotno_subjektno_stevilo) cilj (meja - 1)
    in
    pomozna 0 1 cilj 15000000  (* meja je samo zato, če se slučajno zacikla (ampak se ne) *)


let uporabi_transformacijo_n_krat (start : int) (n : int) (subjektno_stevilo : int) : int =
    let rec pomozna' (zacetek : int) (indeks : int) : int =  (* spet repna rekurzija*)
        if indeks = 0 then
            zacetek
        else
            pomozna' (transformacija zacetek subjektno_stevilo) (indeks - 1)
    in
    pomozna' start n


(* ---------------------------------------- *)


let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list = (
    String.split_on_char '\n' vsebina
)


let vrni_vrstici (seznam : 'a list) : ('a * 'a) =
    if not (List.length seznam = 2) then
        failwith "Seznam ni dolžine 2!"
    else
        (List.nth seznam 0, List.nth seznam 1)


let naloga1 (vsebina : string) : string =
    let javni_kljuc_1, javni_kljuc_2 = vsebina |> prevedi_vsebino_datoteke_v_seznam |> List.map int_of_string |> vrni_vrstici
    in
    let velikost_zanke_1 = pridobi_velikost_zanke javni_kljuc_1
    in
    let velikost_zanke_2 = pridobi_velikost_zanke javni_kljuc_2
    in
    (* Tole dvoje je enako, namreč množenje je komutativno, modulo pa ohranja to (vseeno je, a najprej množimo in potem gledamo ostanek, ali obratno). Skratka, modulo je homomorfizem *)
    let rezultat_1 = uporabi_transformacijo_n_krat 1 velikost_zanke_1 javni_kljuc_2  (* 1 samo zato, ker se postopek pač začne z 1, kot piše v navodilih *)
    in
    let rezultat_2 = uporabi_transformacijo_n_krat 1 velikost_zanke_2 javni_kljuc_1
    in
    if not (rezultat_1 = rezultat_2) then
        failwith "To se ne more zgoditi"
    else
        string_of_int rezultat_1
    (* 
    To troje je vse enako, ampak to ni rešitev:
    let rezultat_3 = uporabi_transformacijo_n_krat javni_kljuc_1 (velikost_zanke_2) 7
    let rezultat_4 = uporabi_transformacijo_n_krat javni_kljuc_2 (velikost_zanke_1) 7
    let rezultat_5 = uporabi_transformacijo_n_krat 1 (velikost_zanke_1 + velikost_zanke_2) 7
    Drugič bolje beri navodila
    *)


(* ---------------------------------------- *)


let () = Sys.chdir "Resitve"
let () = Sys.chdir "25"


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
    let vsebina_datoteke = preberi_datoteko "day_25.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    (* and odgovor2 = naloga2 vsebina_datoteke *)
    in
    izpisi_datoteko "day_25_1.out" odgovor1;
    (* izpisi_datoteko "day_25_2.out" odgovor2 *)