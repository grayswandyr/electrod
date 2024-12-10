  $ for f in *.elo; do echo "----- $f -----"; electrod -v --ln --of=plain -t nuXmv $f; done
  $ for f in *.elo; do echo "----- $f -----"; electrod -v --ln --of=plain -t NuSMV $f; done
  $ for f in *.elo; do echo "----- $f (BMC) -----"; electrod -v --ln --of=plain -t nuXmv --bmc 5 $f; done
  $ for f in *.elo; do echo "----- $f (BMC) -----"; electrod -v --ln --of=plain -t NuSMV --bmc 5 $f; done
