if [[ -o interactive ]]; then
  [[ ! -r /home/fengde/SAIL/.opam/opam-init/complete.zsh ]] || source /home/fengde/SAIL/.opam/opam-init/complete.zsh  > /dev/null 2> /dev/null

  [[ ! -r /home/fengde/SAIL/.opam/opam-init/env_hook.zsh ]] || source /home/fengde/SAIL/.opam/opam-init/env_hook.zsh  > /dev/null 2> /dev/null
fi

[[ ! -r /home/fengde/SAIL/.opam/opam-init/variables.sh ]] || source /home/fengde/SAIL/.opam/opam-init/variables.sh  > /dev/null 2> /dev/null
