###: https://just.systems/man/en/

default:
  @just --choose

##: feedback
icon-ok := '✔'
msg-ok := icon-ok + " OK"
msg-done := icon-ok + " Done"

##: binary cache
cachix-cache-name := 'dotfield'
cachix-jobs := '4'
cachix-exec := "cachix watch-exec " + cachix-cache-name + " --jobs " + cachix-jobs

##: directories/paths
prj-root := env_var('PRJ_ROOT')
prj-data := env_var('PRJ_DATA_HOME')

###: EMACS =====================================================================

# <- Evaluate elisp via `emacsclient`
eval elisp:
  emacsclient --no-wait --eval "{{elisp}}"

###: LINTING/FORMATTING ========================================================

# TODO: currently only handles nix files

deadnix-params := '--no-underscore'

# <- Lint files
# lint:


# <- Write linter fixes to files
# fix:


# <- Lint and format files
fmt:
  fd --extension nix --exec-batch \
    nixfmt --width=$NIXFMT_MAX_WIDTH
