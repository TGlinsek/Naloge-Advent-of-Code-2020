let prevedi_vsebino_datoteke_v_seznam_stevil (vsebina : string) : int list =
    (String.split_on_char '\n' vsebina) |> List.map int_of_string


let rec obrni_seznam (sez : 'a list) : 'a list =  (* seznam obrnemo, saj se elementi dodajajo iz leve in ne iz desne *)
    match sez with
    | [] -> []
    | x :: ostanek -> (obrni_seznam ostanek) @ [x]


let rec vrni_indeks_ponovitve (sez : int list) (element : int) (stevec : int) : int =
    match sez with
    | x :: ostanek -> if x = element then stevec else vrni_indeks_ponovitve ostanek element (stevec + 1)
    | [] -> 0


let naslednje_stevilo (sez_rezultatov : int list) : int =
    match sez_rezultatov with
    | x :: ostanek -> vrni_indeks_ponovitve ostanek x 1
    | [] -> failwith "To se ne bi smelo zgoditi."


let rec napolni_seznam (sez : int list) : int =
    match List.length sez with
    | 2020 -> List.nth sez 0
    | _ -> napolni_seznam ((naslednje_stevilo sez) :: sez)


let naloga1 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam_stevil |> obrni_seznam |> napolni_seznam |> string_of_int


let () = Sys.chdir "Resitve"
let () = Sys.chdir "15"


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
    let vsebina_datoteke = preberi_datoteko "day_15.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    (* and odgovor2 = naloga2 vsebina_datoteke *)
    in
    izpisi_datoteko "day_15_1.out" odgovor1;
    (* izpisi_datoteko "day_11_2.out" odgovor2 *)