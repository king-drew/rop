# Railway Oriented Programming

A project to follow along with the article at https://fsharpforfunandprofit.com/posts/recipe-part2/

| Concept | Description |
| ------- | ----------- |
| succeed | A constructor that takes a one-track value and creates a two-track value on the Success branch. In other contexts, this might also be called return or pure.
| fail | constructor that takes a one-track value and creates a two-track value on the Failure branch.
bind | An adapter that takes a switch function and creates a new function that accepts two-track values as input.
| >= | An infix version of bind for piping two-track values into switch functions.
| >> | composition. A combiner that takes two normal functions and creates a new function by connecting them in series.
| >=> | Switch composition. A combiner that takes two switch functions and creates a new switch function by connecting them in series.
| switch | An adapter that takes a normal one-track function and turns it into a switch function. (Also known as a "lift" in some contexts.)
| map | An adapter that takes a normal one-track function and turns it into a two-track function. (Also known as a "lift" in some contexts.)
| tee | adapter that takes a dead-end function and turns it into a one-track function that can be used in a data flow. (Also known as tap.)
| tryCatch | An adapter that takes a normal one-track function and turns it into a switch function, but also catches exceptions.
| doubleMap | An adapter that takes two one-track functions and turns them into a single two-track function. (Also known as bimap.)
| plus | A combiner that takes two switch functions and creates a new switch function by joining them in "parallel" and "adding" the results. (Also known as ++ and <+> in other contexts.)
| &&& | The "plus" combiner tweaked specifically for the validation functions, modelled on a binary AND.
