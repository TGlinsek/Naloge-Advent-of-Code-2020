let prevedi_vsebino_datoteke_v_seznam (vsebina : string) : string list =
    String.split_on_char '\n' vsebina


let iz_stringa_v_seznam_charov (s : string) : char list =
    let rec pomozna_funkcija (s' : string) (indeks : int) : char list =
        match indeks with
        | a when a = String.length s' -> []
        | _ -> (s'.[indeks] :: (pomozna_funkcija s' (indeks + 1)))
    in
    pomozna_funkcija s 0


type ukazi = 
    | Mask of string
    | Mem of {naslov : string; vrednost : string}


let vrni_ustrezen_tip (vrstica : string) : ukazi =
    let razdeljena_vrstica = String.split_on_char ' ' vrstica in
    let prvi_del = List.nth razdeljena_vrstica 0 in
    if "mask" = prvi_del then
        Mask (List.nth razdeljena_vrstica 2)
    else  (* mem *)
        let drugi_del_z_zaklepajem = List.nth (String.split_on_char '[' prvi_del) 1 in
        Mem {
            naslov = String.sub drugi_del_z_zaklepajem 0 ((String.length drugi_del_z_zaklepajem) - 1);
            vrednost = List.nth razdeljena_vrstica 2
        }


let rec element_v_seznamu (element : 'a) (seznam : 'a list) : bool =
    match seznam with
    | [] -> false
    | x :: ostanek -> (x = element) || (element_v_seznamu element ostanek)


(* v spominu gremo od leve proti desni, saj so na levi najnovejše spremembe, na desni pa najstarejše. Imamo še seznam ponovitev, da vsak naslov vzamemo le enkrat *)
let prestej (spomin : (string * int) list) : int =
    let rec pomozna_funkcija (spomin : (string * int) list) (seznam_ponovitev : string list) : int =
        match spomin with
        | par :: ostanek -> (
            let naslov = fst par
            in
            let vrednost = snd par
            in
            if not (element_v_seznamu naslov seznam_ponovitev) then  (* naslov ni v seznamu_ponovitev *)
                vrednost + pomozna_funkcija ostanek (naslov :: seznam_ponovitev)
            else
                pomozna_funkcija ostanek seznam_ponovitev
        )
        | [] -> 0
    in
    pomozna_funkcija spomin []


let rec (^) (osnova : int) (potenca : int) : int =
    match potenca with
    | m when m < 0 -> failwith "Ta funkcija ne računa negativnih potenc!"
    | 0 -> 1
    | n -> osnova * (osnova ^ (potenca - 1))


let preberi_bit (stevilo : int) (mesto : int) : int =  (* najbolj desno je mesto 0 *)
    let potenca = 2 ^ mesto
    in
    ((stevilo - (stevilo mod potenca)) mod (2 * potenca)) / potenca


let spremeni_vrednost (mask : string) (vrednost : string) : int =  (* vrne decimalno vrednost rezultata *)
    let mask' = iz_stringa_v_seznam_charov mask
    in
    let pretvorjena_vrednost = int_of_string vrednost
    in
    let rec vrni_maskirano_stevilo (maska : char list) (stevilo : int) (mesto : int) : int =
        match maska with
        | znak :: ostanek -> (
            let preostanek = preberi_bit stevilo mesto
            in
            let naslednje_stevilo = (
                if znak = '0' then
                    stevilo - preostanek * (2 ^ mesto)
                else if znak = '1' then
                    stevilo + (1 - preostanek) * (2 ^ mesto)
                else if znak = 'X' then
                    stevilo
                else 
                    failwith "Neveljaven znak v masku!"
            )
            in
            vrni_maskirano_stevilo ostanek naslednje_stevilo (mesto - 1)
        )
        | [] -> stevilo
    in
    vrni_maskirano_stevilo mask' pretvorjena_vrednost 35


let zazeni_program (vsebina : string list) =
    let rec pomozna (vsebina : string list) (trenutni_mask : string) (trenutni_spomin : (string * int) list) : int =
        match vsebina with
        | vrstica :: preostanek -> (
            match vrni_ustrezen_tip vrstica with
            | Mask niz -> (pomozna preostanek niz trenutni_spomin)
            | Mem par -> (
                let naslov = par.naslov
                in
                let vrednost = par.vrednost
                in 
                let nov_spomin = (naslov, (spremeni_vrednost trenutni_mask vrednost))
                in
                pomozna preostanek trenutni_mask (nov_spomin :: trenutni_spomin)  (* novejše vnose dodajamo na levo *)
            )
        )
        | [] -> prestej trenutni_spomin
    in
    pomozna vsebina "Tu je lahko karkoli." []


let naloga1 (vsebina : string) : string =
    vsebina |> prevedi_vsebino_datoteke_v_seznam |> zazeni_program |> string_of_int


let () = Sys.chdir "Resitve"
let () = Sys.chdir "14"


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
    let vsebina_datoteke = preberi_datoteko "day_14.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    (* and odgovor2 = naloga2 vsebina_datoteke *)
    in
    izpisi_datoteko "day_14_1.out" odgovor1;
    (* izpisi_datoteko "day_11_2.out" odgovor2 *)