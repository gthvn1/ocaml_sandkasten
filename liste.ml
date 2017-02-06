type 'a liste =
    | ListeVide
    | ListeNonVide of 'a * 'a liste;;

let cons tete queue = ListeNonVide (tete, queue);;


