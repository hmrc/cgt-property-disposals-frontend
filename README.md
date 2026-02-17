# cgt-property-disposals-frontend

This is a microservice which provides a UI for reporting Capital Gains Tax on UK property (CGT). This service is written
in Scala using the Play framework. It provides functionality to do the following:

- subscribe to the CGT service
- manage subscription details
- fill in and submit CGT returns
- amend a return which has been submitted online
- pay any charges due

Subscription details and CGT returns are held in the Enterprise Tax Management Platform (ETMP) which is a HMRC back end
system. Details are collected in this frontend microservice and then relayed on to ETMP via a
[backend microservice](https://github.com/hmrc/cgt-property-disposals). Access to this service requires users to be
logged in via Government Gateway. The service can be access in
production [here](https://www.tax.service.gov.uk/capital-gains-tax-uk-property/start/report-pay-capital-gains-tax-uk-property).

The service also allows agents to manage subscription details and submit returns on behalf of clients. An agent-client
handshake has to have been completed before agents can perform such actions on behalf of clients. The handshake process
is managed by a separate service. Users can also submit a return on behalf someone whilst logged into their own
CGT account. In this case, the user is not an agent, but they are either capacitors, personal representatives, or they
represent an estate of the property depending on whether the person being represented is deceased or not and if so
whether
the disposal of the property was made before after the person died.

The entry point into the service is the start endpoint `GET /start`. This will take you through the onboarding journey
if the user logged in hasn't subscribed yet or it will take you to the account home page if the user has already logged
in. In the code, this endpoint sets the initial `JourneyState`. `JourneyState` is a sealed
trait which describes a particular journey, e.g. subscribing to CGT, filling out a return or viewing a return. It
controls
the pages a user can see and changes as needed as a user uses the service.

## Onboarding

The onboarding journey consists of collection of user data to create a CGT account. The data collected are name, postal
address and email address.

We attempt to automatically retrieve user details in ETMP using an identifier associated with the user's Government
Gateway
account. The API used to retrieve this data is called the Register with ID API. If we can retrieve the data, the
retrieved
details will be used as a baseline set of details the user can use to subscribe to CGT with. The user can choose to
supply
other details if they would like to subscribe to CGT with different details. This is known as the Register with ID
route.
If we cannot retrieve details then we manually capture the details to subscribe to CGT with. This is called the Register
without ID route. This route is only available to non-UK residents.

## Returns

Once a user has subscribed they will have the ability to submit returns. Broadly speaking, a return journey consists of
the following:

- answering a series of questions in an initial "triage" section. This section determines whether a user can use
  this service to report their CGT tax.
- if we determine that the usr can use our service, they are presented with a list of tasks to do. This is called the "
  task
  list". Each task corresponds to different sections of the return which consist of questions the user will have to
  answer.
  Each section has a "check your answers" page where users can review their answers to the questions in that section.
- once each task is complete, the user can check all their answers and submit the return

After the triage section has been completed, the return is saved as a draft return. While the return is incomplete and
has not been sent, users can log out at any time and access the draft return via the account homepage for a set period
of time. Any changes to return are automatically saved as the questions are answered.

After a user has submitted a return they will be able to view the return from the account home page and pay any tax due.
They will also be able to amend the return if they wish to do so.

The questions in the return will depend on the number of assets that have been disposed of and the asset type(s). This
results in different task lists depending on the type of journey.

In some returns we will be able to calculate the amount of tax due to help users. In these cases, we can display what we
think
the user owes and show how we did the calculation. The user can agree with the calculated figure or enter a different
amount. In
the cases where we cannot perform a calculation, the user will have to calculate it themselves and supply us with a
figure.

Users can upload files to support their return if they wish to. If a user supplied their own figure of how much tax they
owe then it is mandatory for them to upload a file to show how they calculated it.

## How to run

Use `sbt run` in a terminal to run this service. This service runs on port `7020` by default.

The other services this service relies on can be run using the `CGTPD_ALL` service manager profile and stopping the
`CGTPD_FRONTEND` service if you want to run this frontend service from source, e.g.:

```
sm2 --start CGTPD_ALL
sm2 --stop CGTPD_FRONTEND
```   

Controller property-based specs (`ControllerSpec`) default to:

- `minSuccessful = 6`
- `sizeRange = 5`

For stricter local runs, override with JVM system properties, for example:

```
sbt -DcontrollerSpec.minSuccessful=10 -DcontrollerSpec.sizeRange=6 test
```

For details on accessing the service on both local and higher up environments, please refer to 
[CGT Development](https://confluence.tools.tax.service.gov.uk/display/DDCWLS/CGT%3A+Development) confluence page.

### License

This code is open source software licensed under
the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").
