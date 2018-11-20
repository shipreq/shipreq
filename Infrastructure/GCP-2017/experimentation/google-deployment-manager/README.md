This is shit.

Can't see what changes it'll make before it makes them.

Preview is useless, just uploads your config without executing.

Will silently DELETE THE DATABASE if it feels it needs to.

Doesn't even validate the config when it's in preview mode, only when you execute does it tell you that your config is wrong.

Everything about it feels like a buggy hack.

Have to create vs update

Preview is some weird state. You apply a new preview on top, it's so stupid.

Will use Terraform instead. Terraform isn't perfect but it's so much better:
* terraform plan validates config
* terraform plan validates tells you what you'll get
* terraform plan validates tells you what it will do in an update
* terraform apply either creates or updates as required
