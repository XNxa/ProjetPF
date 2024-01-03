# Nos etat de notre jeu

```OCaml
type etat = Etat of etat_balle * etat_briques * etat_raquette * score

type vitesse = float * float
type position = float * float
type etat_balle = position * vitesse

type score = int

type etat_briques = etat_brique list
type sante_brique = Entiere | Casse 
type brick_color = Rouge | Vert | Jaune | Rose
type etat_brique = int * int * sante_brique * brick_color

type etat_raquette = Position of int


(* Dans un module : un foncteur alors *)
module type Quadtree =
sig
    type a
    type a quadtree = 
        | Leaf of a
        | Node of quadtree * quadtree * quadtree * quadtree

    val profondeur_max : a quadtree -> int (* maillage *)
    val get_feuille : int -> int -> a quadtree -> a
    val set_feuille : int -> int -> 
end
```
