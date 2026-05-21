# ShipReq

In 2013 I began work on an ambitious project to rethink how requirements are stored and maintained.
I've now chosen to open-source this, my life's work and magnum opus.

# What is ShipReq?

[ShipReq](https://github.com/shipreq/shipreq) is a SaaS/webapp for storing and managing project requirements, tasks, and issues.
It's like a highly configurable combination of JIRA, Excel, and Word, plus new and innovative features not found anywhere else.

It was born out of frustration working in an environment where Excel and Word were used for requirements, and everything fell out of date, and was hard to keep correct.
ShipReq makes certain classes of errors impossible, and other types of errors trackable.
For example, you can write Use Cases such that all the links between steps always stay correct as intended;
you even get a graph of steps to make it easier to comprehend.

It's written in full-stack [Scala](https://www.scala-lang.org) using [Scala.js](https://www.scala-js.org/), and is nearly all pure functional programming.
It is extremely well tested, both the backend and the UI.
The main frontend is a massive SPA which only talks to the server to receive real-time updates or save changes, meaning that all navigation happens locally and is always very fast.
Rather than apps like JIRA loading an issue at a time, ShipReq is more like git in that the entire repo is loaded client-side.

# Demos

<a href="https://youtu.be/YjdI_dufSUI" target="_blank">
  <img src="https://img.youtube.com/vi/YjdI_dufSUI/hqdefault.jpg" width="260" alt="Watch the video" />
</a>
<a href="https://youtu.be/5ytrBxUoStc" target="_blank">
  <img src="https://img.youtube.com/vi/5ytrBxUoStc/hqdefault.jpg" width="260" alt="Watch the video" />
</a>
<a href="https://youtu.be/GgqBY6u9yQg" target="_blank">
  <img src="https://img.youtube.com/vi/GgqBY6u9yQg/hqdefault.jpg" width="260" alt="Watch the video" />
</a>

# Running Locally

[Details are here](./Code/doc/running.md).

# Current State

There are two apps: the webapp and taskman.

The webapp works great.

Taskman is the service that is supposed to run all background tasks like sending emails, updating mailing lists, and raising tickets in an external support-desk platform.
All of this functionality is now [configured (in dev)](./Code/envs/dev/taskman/shipreq.properties) to be off, meaning it just logs and does nothing.
This is because the third-party integration APIs have all changed over the years, and I haven't spent the time to upgrade.
For email sending, both the MailGun integration and Java mail (via SMTP) should still work.
For running locally with no email-sending or third-party integrations, this is all fine.

(Upgrading these third-party APIs would make an excellent first issue for anyone looking to contribute!)

In terms of features, it always needs more.
I'm just releasing this as-is, rather than waiting until all desirable features are added.
You may or may not find the existing feature set lacking; it depends on how you choose to configure and use it.
Feel free to suggest features that you're needing.

# Journey and Crisis

I started work on this back in 2013. I wanted to both take a chance at self-employment, and solve problems I'd experienced working with large teams.
Young and naive, my plan was to live off my savings, create the perfect product, and then seek paid users.
It was important to me to get the foundations right, rather than churn out a quick MVP.

I open-sourced a lot of my work along the way, resulting in libraries like [scalajs-react](https://github.com/japgolly/scalajs-react), [scalacss](https://github.com/japgolly/scalacss), [test-state](https://github.com/japgolly/test-state), [etc](https://japgolly.github.io/japgolly/).

And so I worked on ShipReq and OSS for a few years, did a few years of paid work to pay the bills, then covid hit and I spent years working on this again.

After the covid years, a number of areas of my life collapsed. There were multiple family tragedies (including deaths), I went through a bad divorce, my health suffered and I went to hospital more times than I can count, and I was diagnosed as having multiple disabilities.
I ended up taking two years off from programming and blew through all of my life's savings.
I've been looking for a job for over eight months since, with no luck, partly because my interview skills are impacted by my disabilities, and partly because my recent years have been dedicated to open-source and independent R&D rather than traditional corporate employment.
I fear I may never get a job again.

Now I find myself on the disability pension with an abundance of free time on my hands, and no life purpose.
I've therefore decided to continue work on ShipReq, and to open-source it.
Open-sourcing wasn't an easy decision but it's the only one that makes sense, seeing as I no longer have the wherewithal to put on my business hat and seek paid users.
To be honest, I can't even afford to host it anymore.

ShipReq's goal was to bring joy to the world. Hopefully by making it open and free, there are people out there that will benefit from it and enjoy it. There's a lot of attention to detail to appreciate.

# Appeal

* 💖 **Sponsor / Donate:** If my open-source work has helped your projects or your company, please consider supporting my continued work via [Patreon](https://www.patreon.com/japgolly).

* 🌐 **Hosting Support:** I can no longer afford to host ShipReq. If an organisation or individual would like to fund or provide infrastructure to host a public instance, please [email me](mailto:japgolly@gmail.com) to discuss.

* 🛠️ **Feature Sponsorship:** If your team wants to use ShipReq but needs specific features, please [email me](mailto:japgolly@gmail.com) to discuss.

* 💼 **Hiring:** I am actively looking for remote roles in Scala, Scala.js, or Java. If your team values deep technical expertise, meticulous testing, and pure functional programming, please [reach out](mailto:japgolly@gmail.com).
