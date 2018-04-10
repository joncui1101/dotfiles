function kill-ssh {
    ps aux | grep "ssh " | grep -v "grep" | awk '{print $2}' | xargs kill -9
}

vg() {
  local file

  file="$(ag --nobreak --noheading $@ | fzf -0 -1 | awk -F: '{print $1 " +" $2}')"

  if [[ -n $file ]]
  then
     v $file
  fi
}
