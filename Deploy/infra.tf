provider "google" {
  credentials = "${file(".terraform-key.json")}"
  project     = "shipreq-dev"
  region      = "australia-southeast1"
}

resource "google_container_cluster" "apps" {
  name               = "apps"
  zone               = "australia-southeast1-b"
  initial_node_count = 2

  node_config {
    disk_size_gb = 10
    machine_type = "custom-2-2560"
    image_type   = "COS"

    oauth_scopes = [
      "https://www.googleapis.com/auth/compute",
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/logging.write",
      "https://www.googleapis.com/auth/monitoring.write",
      "https://www.googleapis.com/auth/service.management.readonly",
      "https://www.googleapis.com/auth/servicecontrol",
      "https://www.googleapis.com/auth/trace.append",
    ]
  }

  addons_config {
    http_load_balancing {
      disabled = true
    }

    horizontal_pod_autoscaling {
      disabled = true
    }
  }
}
