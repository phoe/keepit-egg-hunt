# Keepit Egg Hunt

This is the Keepit Egg Hunt repository, published as a reference
for the upcoming European Lisp Symposium 2025 paper.

© Keepit A/S 2025. All rights reserved.

This code is licensed as source-available for the time being.

## Running

There are two principal ways to run the Egg Hunt code:

### Loading into running image

Once inside a running image, load (*not* compile-and-load) the file
`egg-hunt.lisp` to avoid compiling in too many spoilers. Either `(load
...)` the file from the REPL or use Slime's `C-c C-l` when visiting
the file.

### Preparing custom Lisp image

Run SBCL with the `egg-hunt.lisp` loaded:

```
sbcl --load egg-hunt.lisp
```

and then build the image from the REPL:

```lisp
(keepit-egg-hunt::build)
```

This should result in an image file at `/tmp/keepit-egg-hunt`.

#### Running custom image in Slime

The custom image file can be used as just another Lisp image from
within Slime. To make it available, tweak your
`slime-lisp-implementations` variable to include a config pointing at
the image, e.g.:

```lisp
(setf slime-lisp-implementations 
    '((egghunt-sbcl ("/path/to/keepit-egg-hunt"))
      ...))
```

This will make `egghunt-sbcl` one of the images you can run from
Slime. To explicitly select it, give a negative prefix to Slime,
i.e. run `M-- M-x slime` and then select it.
