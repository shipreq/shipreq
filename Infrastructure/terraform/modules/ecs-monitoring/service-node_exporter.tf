locals {
  node_exporter_tags = merge(var.default_tags, { Name = "${var.name_prefix}-node_exporter" })

  node_exporter_args = [

    # Disable on-by-default collectors
    "--no-collector.arp",       # Exposes ARP statistics from /proc/net/arp. 	Linux
    "--no-collector.bcache",    # Exposes bcache statistics from /sys/fs/bcache/. 	Linux
    "--no-collector.bonding",   # Exposes the number of configured and active slaves of Linux bonding interfaces. 	Linux
    "--no-collector.conntrack", # Shows conntrack statistics (does nothing if no /proc/sys/net/netfilter/ present). 	Linux
    # "--no-collector.cpu",          # Exposes CPU statistics 	Darwin, Dragonfly, FreeBSD, Linux, Solaris
    # "--no-collector.cpufreq",      # Exposes CPU frequency statistics 	Linux, Solaris
    # "--no-collector.diskstats",    # Exposes disk I/O statistics. 	Darwin, Linux, OpenBSD
    "--no-collector.edac",    # Exposes error detection and correction statistics. 	Linux
    "--no-collector.entropy", # Exposes available entropy. 	Linux
    # "--no-collector.exec",         # Exposes execution statistics. 	Dragonfly, FreeBSD
    # "--no-collector.filefd",       # Exposes file descriptor statistics from /proc/sys/fs/file-nr. 	Linux
    # "--no-collector.filesystem",   # Exposes filesystem statistics, such as disk space used. 	Darwin, Dragonfly, FreeBSD, Linux, OpenBSD
    "--no-collector.hwmon",      # Expose hardware monitoring and sensor data from /sys/class/hwmon/. 	Linux
    "--no-collector.infiniband", # Exposes network statistics specific to InfiniBand and Intel OmniPath configurations. 	Linux
    "--no-collector.ipvs",       # Exposes IPVS status from /proc/net/ip_vs and stats from /proc/net/ip_vs_stats. 	Linux
    # "--no-collector.loadavg",      # Exposes load average. 	Darwin, Dragonfly, FreeBSD, Linux, NetBSD, OpenBSD, Solaris
    "--no-collector.mdadm", # Exposes statistics about devices in /proc/mdstat (does nothing if no /proc/mdstat present). 	Linux
    # "--no-collector.meminfo",      # Exposes memory statistics. 	Darwin, Dragonfly, FreeBSD, Linux, OpenBSD
    # "--no-collector.netclass",     # Exposes network interface info from /sys/class/net/ 	Linux
    # "--no-collector.netdev",       # Exposes network interface statistics such as bytes transferred. 	Darwin, Dragonfly, FreeBSD, Linux, OpenBSD
    # "--no-collector.netstat",      # Exposes network statistics from /proc/net/netstat. This is the same information as netstat -s. 	Linux
    # "--no-collector.nfs",  # Exposes NFS client statistics from /proc/net/rpc/nfs. This is the same information as nfsstat -c. 	Linux
    # "--no-collector.nfsd", # Exposes NFS kernel server statistics from /proc/net/rpc/nfsd. This is the same information as nfsstat -s. 	Linux
    "--no-collector.pressure", # Exposes pressure stall statistics from /proc/pressure/. 	Linux (kernel 4.20+ and/or CONFIG_PSI)
    # "--no-collector.schedstat",    # Exposes task scheduler statistics from /proc/schedstat. 	Linux
    # "--no-collector.sockstat",     # Exposes various statistics from /proc/net/sockstat. 	Linux
    # "--no-collector.stat",         # Exposes various statistics from /proc/stat. This includes boot time, forks and interrupts. 	Linux
    "--no-collector.textfile", # Exposes statistics read from local disk. The --collector.textfile.directory flag must be set. 	any
    "--no-collector.time",     # Exposes the current system time. 	any
    "--no-collector.timex",    # Exposes selected adjtimex(2) system call stats. 	Linux
    "--no-collector.uname",    # Exposes system information as provided by the uname system call. 	Darwin, FreeBSD, Linux, OpenBSD
    # "--no-collector.vmstat",       # Exposes statistics from /proc/vmstat. 	Linux
    "--no-collector.xfs", # Exposes XFS runtime statistics. 	Linux (kernel 4.4+)
    "--no-collector.zfs", # Exposes ZFS performance statistics. 	Linux, Solaris

    # Enable off-by-default collectors
    # "--collector.buddyinfo",    # Exposes statistics of memory fragments as reported by /proc/buddyinfo. 	Linux
    # "--collector.devstat",      # Exposes device statistics 	Dragonfly, FreeBSD
    # "--collector.drbd",         # Exposes Distributed Replicated Block Device statistics (to version 8.4) 	Linux
    # "--collector.interrupts",   # Exposes detailed interrupts statistics. 	Linux, OpenBSD
    # "--collector.ksmd",         # Exposes kernel and system statistics from /sys/kernel/mm/ksm. 	Linux
    # "--collector.logind",       # Exposes session counts from logind. 	Linux
    # "--collector.meminfo_numa", # Exposes memory statistics from /proc/meminfo_numa. 	Linux
    # "--collector.mountstats",   # Exposes filesystem statistics from /proc/self/mountstats. Exposes detailed NFS client statistics. 	Linux
    # "--collector.ntp",          # Exposes local NTP daemon health to check time 	any
    # "--collector.perf",         # Exposes perf based metrics (Warning: Metrics are dependent on kernel configuration and settings). 	Linux
    # "--collector.processes",    # Exposes aggregate process statistics from /proc. 	Linux
    # "--collector.qdisc",        # Exposes queuing discipline statistics 	Linux
    # "--collector.runit",        # Exposes service status from runit. 	any
    # "--collector.supervisord",  # Exposes service status from supervisord. 	any
    # "--collector.systemd",      # Exposes service and system status from systemd. 	Linux
    # "--collector.tcpstat",      # Exposes TCP connection status information from /proc/net/tcp and /proc/net/tcp6. (Warning: the current version has potential performance issues in high load situations.) 	Linux
    # "--collector.wifi",         # Exposes WiFi device and station statistics. 	Linux

    "--path.rootfs=/rootfs",
    "--path.procfs=/host/proc",
    "--path.sysfs=/host/sys",
    "--collector.filesystem.ignored-mount-points=^/(sys|proc|dev|host|etc)($$|/)"
  ]
}

resource "aws_ecs_service" "node_exporter" {
  name                = "${var.name_prefix}-node_exporter"
  cluster             = var.cluster_id
  task_definition     = aws_ecs_task_definition.node_exporter.arn
  scheduling_strategy = "DAEMON"
  propagate_tags      = "SERVICE"
  tags                = local.node_exporter_tags
}

resource "aws_ecs_task_definition" "node_exporter" {
  family = "${var.name_prefix}-node_exporter"
  tags   = local.node_exporter_tags

  container_definitions = <<EOB
[
  {
    "name": "${var.name_prefix}-node_exporter",
    "image": "${var.node_exporter_image}",
    "privileged": true,
    "command": ${jsonencode(local.node_exporter_args)},
    "mountPoints": [
      {
        "sourceVolume": "rootfs",
        "containerPath": "/rootfs",
        "readOnly": true
      },
      {
        "sourceVolume": "proc",
        "containerPath": "/host/proc",
        "readOnly": true
      },
      {
        "sourceVolume": "sys",
        "containerPath": "/host/sys",
        "readOnly": true
      }
    ],
    "portMappings": [
      {
        "protocol": "tcp",
        "hostPort": ${var.node_exporter_port},
        "containerPort": 9100
      }
    ],
    "cpu": ${var.node_exporter_cpu},
    "memoryReservation": ${var.node_exporter_mem_res},
    "memory": 92,
    "healthCheck": {
      "command": [
        "CMD-SHELL",
        "wget -qO - http://localhost:9100/metrics | fgrep -q '\"} ' || exit 1"
      ],
      "startPeriod": ${local.healthcheck.startPeriod},
      "interval": ${local.healthcheck.interval},
      "timeout": ${local.healthcheck.timeout},
      "retries": ${local.healthcheck.retries}
    }
  }
]
EOB

  volume {
    name      = "rootfs"
    host_path = "/"
  }

  volume {
    name      = "proc"
    host_path = "/proc"
  }

  volume {
    name      = "sys"
    host_path = "/sys"
  }
}
