#!/bin/bash

cat <<'EOB' >> /etc/ecs/ecs.config
ECS_CLUSTER=${cluster}
EOB
