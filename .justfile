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

schemastore-catalog-url := 'https://raw.githubusercontent.com/SchemaStore/schemastore/refs/heads/master/src/api/json/catalog.json'
schemastore-catalog-file := prj-root / "data/json-schema/catalog.json"

###: EMACS =====================================================================

# <- Evaluate elisp via `emacsclient`
eval elisp:
  emacsclient --no-wait --eval "{{elisp}}"

update-schemastore:
  curl {{schemastore-catalog-url}} \
  | jq -c '.schemas | map({fileMatch, url})' \
  > {{schemastore-catalog-file}}

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
