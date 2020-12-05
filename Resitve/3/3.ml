let pridobi_seznam vsebina =
    (* Str.split (vsebina '\n') *)
    String.split_on_char '\n' vsebina  (* char je v enojnih narekovajih *)


let pridobi_znak niz indeks =
    String.get niz indeks


let dolzina_prve_vrstice sez =
    String.length (List.nth sez 0)


let rec prestej_drevesa (sez : string list) (odmik : float) (korak : float) : int = (
    match sez with
    | [] -> 0
    | x :: ostanek -> (
        let navzdol_zaokrozen = Float.rem odmik 1.
        in
        (
            if navzdol_zaokrozen <> 0. 
                then 0
            else 
                (
                    let celo_stevilo = int_of_float odmik
                    in
                    (
                        if pridobi_znak x (
                            celo_stevilo mod (dolzina_prve_vrstice sez)
                        ) = '#' 
                            then (
                                1
                            )
                        else 
                            0
                    )
                )
        ) 
        + 
        (
        prestej_drevesa 
            ostanek 
            (odmik +. korak)
            korak
        )
    )
)


let koliko_dreves sez korak =
    prestej_drevesa sez 0. korak


(*
let rec prestej_drevesa (sez : string list) (odmik : int) : int = (
    match sez with
    | [] -> 0
    | x :: ostanek -> (
        (if pridobi_znak x (odmik mod (dolzina_prve_vrstice sez)) = '#' then 1 else 0) + (prestej_drevesa ostanek (odmik + 3))
    )
*)


let naloga1 (vsebina_datoteke : string) =
    string_of_int (
        koliko_dreves (pridobi_seznam vsebina_datoteke) 
        3.0
    )


let naloga2 (vsebina_datoteke : string) = 
    string_of_int (
        let sez = [1.; 3.0; 5.0; 7.0; 0.5]
        in
        let vsebina = pridobi_seznam vsebina_datoteke
        in
        (List.fold_left ( * ) 1 (List.map (koliko_dreves vsebina) sez))
    );;


(*
print_string (Sys.getcwd());;
print_string "\n";;
*)


Sys.chdir "Resitve";;
Sys.chdir "3";;


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
    let vsebina_datoteke = preberi_datoteko "day_3.in" in
    (* print_string vsebina_datoteke; *)
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_3_1.out" odgovor1;
    izpisi_datoteko "day_3_2.out" odgovor2
