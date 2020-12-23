let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
    String.split_on_char '\n' vsebina


let rec razdeli_sez_na_dva_dela_brez_indeksa (sez : 'a list) (indeks_locevanja : int) : ('a list * 'a list) =  (* indeksti člen se bo izgubil *)
    if indeks_locevanja <= 0 then 
        ([], List.tl sez) 
    else
        match sez with
        | x :: ostanek -> (
            let prvi, drugi = razdeli_sez_na_dva_dela_brez_indeksa ostanek (indeks_locevanja - 1)
            in
            (x :: prvi, drugi)
        )
        | [] -> ([], [])


let vrni_seznama_stevil (vsebina : string list) : int list * int list =
    let stevilo_vrstic = List.length vsebina
    in
    if stevilo_vrstic mod 2 = 0 then 
        failwith "Sodo število vrstic!"
    else
        let indeks_prazne_vrstice = (stevilo_vrstic - 1) / 2
        in
        let a, b = razdeli_sez_na_dva_dela_brez_indeksa vsebina indeks_prazne_vrstice
        in
        List.map int_of_string (List.tl a), List.map int_of_string (List.tl b)


let prestej_tocke (kup1 : int list) (kup2 : int list) : int =
    let rec igra (kup1 : int list) (kup2 : int list) : int list =  (* vrne zmagovalčev kup *)
        if kup1 = [] then
            kup2
        else if kup2 = [] then
            kup1
        else
            let karta1 = List.nth kup1 0
            in
            let karta2 = List.nth kup2 0
            in
            let ostanek1 = List.tl kup1
            in
            let ostanek2 = List.tl kup2
            in
            if karta1 > karta2 then  (* izenačenje se ne more zgoditi *)
                igra (ostanek1 @ [karta1; karta2]) ostanek2
            else
                igra ostanek1 (ostanek2 @ [karta2; karta1])  (* dejansko vrstni red parametrov ni važen *)
    in
    let zmagovalchev_seznam = igra kup1 kup2
    in
    let dolzina_seznama = List.length zmagovalchev_seznam  (* bi lahko tudi na začetku, ampak ni važno *)
    in
    let prevrednoten_seznam = List.mapi (fun (indeks : int) (el : int) : int -> el * (dolzina_seznama - indeks)) zmagovalchev_seznam
    in
    List.fold_left (+) 0 prevrednoten_seznam


let obratno_curryiranje (f : 'a -> 'b -> 'c) : ('a * 'b -> 'c) =
    fun (a, b) -> f a b


let naloga1 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> vrni_seznama_stevil |> (prestej_tocke |> obratno_curryiranje) |> string_of_int


let () = Sys.chdir "Resitve"
let () = Sys.chdir "22"


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
    let vsebina_datoteke = preberi_datoteko "day_22.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    (* and odgovor2 = naloga2 vsebina_datoteke *)
    in
    izpisi_datoteko "day_22_1.out" odgovor1;
    (* izpisi_datoteko "day_11_2.out" odgovor2 *)