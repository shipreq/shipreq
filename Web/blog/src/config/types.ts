export interface GoogleAnalytics {
  trackingId: string
  jsUrl     : string
  enabled   : boolean
}

export interface StatCounter {
  project : number
  security: string
  https   : boolean
  jsUrl   : string
  enabled : boolean
}
