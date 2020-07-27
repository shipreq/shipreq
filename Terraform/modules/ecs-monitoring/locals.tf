locals {

  healthcheck = {
    startPeriod = 60
    interval    = 60
    timeout     = 10
    retries     = 2
  }

}
