- lower paranoid level: `sudo sysctl kernel.perf_event_paranoid=1`
- check: `cat /proc/sys/kernel/perf_event_paranoid`
- now you can do:
```sh
perf record -g --call-graph=dwarf _build/default/bin/main.exe
perf report
```
- to compare two implementations do:
```sh
hyperfine --shell=none --warmup 10 _build/default/bin/main_day4_1.exe _build/default/bin/main_day4_2.exe
```

