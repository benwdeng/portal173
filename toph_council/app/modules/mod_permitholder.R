###################################################################################################
#
# Portal 173 - About Us module
#
###################################################################################################

mod_permitholder_ui <- function(id){
  
  # Defining namespace
  ns <- NS(id)
  
  fluidPage(
    tabsetPanel(
      tabPanel(
        value = "ph_explain", title = "1 Explain", 
        fluidPage(
          fluidRow(
            h1("What is a section 173 agreement?"),
            align = "center"
          ),
          tags$p(
            class = "text-center",
            tags$img(
              class = "img-responsive img-rounded center-block", 
              src = "What is a s173 agreement.jpg"
              # style = "max-width: 150px;"
            )
          ),
        )
      ),
      tabPanel(
        value = "ph_process", title = "2 Process", 
        fluidPage(
          fluidRow(
            h1("What is the process?"),
            align = "center"
          ),
          tags$p(
            class = "text-center",
            tags$img(
              class = "img-responsive img-rounded center-block", 
              src = "What is the process.jpg"
              # style = "max-width: 150px;"
            )
          ),
        )
      ),
      tabPanel(
        value = "ph_taC", title = "3 Terms", 
        fluidPage(
          fluidRow(
            h1("Terms and conditions"),
            align = "center"
          ),
          box(
            title = "Standard Terms of Appointment",
            status = "danger",
            width = 12,
            tags$p(
              "This is our agreement with you as required under the 
              Legal Profession Uniform Law (Victoria) (Uniform Law)
              telling you:"
            ),
            tags$ul(
              tags$li(
                "an estimate of total legal costs and expenses and the 
                basis on which our fees are calculated; or"
              ),
              tags$li(
                "total legal costs where our fees are a fixed amount 
                and expenses incurred on your behalf are estimated; 
                and "
              ),
              tags$li(
                "your rights in relation to costs."
              ),
            ),
            tags$strong("The basis on which our fees will be calculated"),
            tags$p("Our fees are calculated in one of three ways:"),
            tags$ol(
              tags$li("as a fixed amount;"),
              tags$li("based on our hourly rates; or"),
              tags$li("a combination of both."),
            ),
            tags$p(
              "Our scope of engagement indicates the basis on which our fees are 
              calculated in relation to your matter."
            ),
            tags$strong("When our fees are fixed"),
            tags$p(
              "Where our fees are fixed, they may be fixed for a task, a
              period of time or on another basis. Our scope of
              engagement indicates the work to which our fixed fee relates."
            ),
            tags$strong("When our fees are calculated based on hourly rates"),
            tags$p(
              "Where our fees are calculated based on hourly rates, we
              calculate those fees in 6-minute units."
            ),
            tags$p(
              "The hourly rates of the lawyer responsible for your matter
              and other staff who may assist in the matter are set out in
              our scope of engagement."
            ),
            tags$p(
              "We generally review our hourly rates in June each year to
              take into account changes in costs and market conditions.
              We will notice you of any changes in the hourly rates as
              soon practicable after the review."
            ),
            tags$strong("Expenses we incur on your behalf"),
            tags$p(
              "In addition to our fees, we will charge you at cost for any
              expenses we incur on your behalf. Such expenses might
              be barrister’s fees, court filing fees, Titles Office fees,
              company search fees and fees payable to legal agents
              and any GST on these expenses."
            ),
            tags$p(
              "If, in providing legal services for your, it becomes
              necessary to engage another law practice (including
              barrister’s) to provide specialist advice or services, we will
              consult you about the terms of this engagement before
              incurring the expense. We will provide you with a
              statement setting out the rates and estimated costs of any
              other law practice we propose to engage as soon as this
              information is available."
            ),
            tags$p(
              "We may request you provide us with payment in advance
              of us incurring an expense on your behalf. These funds
              will be held in our trust account until payment of the
              expense is required."
            ),
            tags$strong("Estimated total legal costs"),
            tags$p(
              "The total legal costs, including expenses, for this matter
              are set out in our scope of engagement."
            ),
            tags$strong("Change to our scope of engagement"),
            tags$p(
              "Where our scope of engagement is for a limited amount of
              work in a matter than changes, we endeavour to inform
              you of the possible total legal costs you could incur so that
              you can make an informed decision about incurring legal
              costs for the limited stage."
            ),
            tags$p(
              "If it becomes necessary to carry out work outside the
              scope of engagement, we will notify you and provide you
              with either a revised price or an estimate of the costs for
              that work."
            ),
            tags$strong("Your rights"),
            tags$p("You have the right to:"),
            tags$ul(
              tags$li("negotiate a costs agreement with us;"),
              tags$li("negotiate a billing method with us;"),
              tags$li(
                "on receipt of a bill from us, request an itemised bill
                within 30 days of receiving a non-itemised bill;"
              ),
              tags$li("discuss any concerns with us about your bill;"),
              tags$li("have our costs assessed if you dispute our legal costs;"),
              tags$li(
                "seek assistance from the Victorian Legal Services
                Commissioner if you dispute our legal costs; and"
              ),
              tags$li("apply to set aside our costs agreement.")
            ),
            tags$strong("Award of costs in court proceedings"),
            tags$p(
              "Costs between disputing parties are determined at the
              discretion of the court which can award all or part of them
              in favour of either party, or not at all. If a party is
              successful, the court may require the other party pay
              some or all of the successful party’s costs. If a party is
              unsuccessful, that party may be required to pay all or
              some of the successful party’s costs."
            ),
            tags$strong("Payment and interest on overdue accounts"),
            tags$p(
              "Unless our scope of engagement states otherwise:"
            ),
            tags$ul(
              tags$li(
                "we will invoice you for the work we have done for you
                either on completing a task, monthly or at regular
                intervals;"
              ),
              tags$li(
                "we require our tax invoice to be paid within 14 days
                of the date of the tax invoice or, where we require
                payment in advance, before we start work."
              )
            ),
            tags$p(
              "If our tax invoice is not paid within 14 days, interest will be
              charged at the cast target rate set by the Reserve Bank of
              Australia plus 2%."
            ),
            tags$p(
              "Where you have provided your credit card details to us,
              you authorise payment from your credit card for the
              amount of our tax invoice in accordance with this costs
              agreement unless trust funds are made available before
              the due date for payment."
            ),
            tags$p(
              "A payment processing fee of 0.66% of the payment
              amount (including GST) applies for all Visa or Mastercard
              credit card payments."
            ),
            tags$strong("Your privacy"),
            tags$p(
              "The information we collect from you will only be used and
              disclosed for the purposes of accurately representing you
              and for meeting our legal obligations. In certain
              circumstances, your information may be disclosed to the
              courts, the Law Institute of Victoria, our insurers, barristers
              and other lawyers. You have the right to access your
              personal information, subject to exceptions governed by
              privacy law. If you fail to provide us with any required
              personal information this may reduce the effectiveness of
              our representation and impact the outcome of your matter."
            ),
            tags$strong("Terminating our engagement"),
            tags$p(
              "You may terminate our engagement in writing at any time.
              We will send you a tax invoice up to the date of the
              termination for an amount of the work then completed
              being the applicable proportion of the agreed price plus
              expenses. Until payment of the amount of your tax
              invoice, and any interest payable, we will retain any
              documents or files we hold on your behalf."
            ),
            tags$strong("Declaration in relation to trust money"),
            tags$p(
              "Trust money received by Centre of Mediation on your
              behalf will be banked in the general trust account operated
              by Centre for Mediation in Victoria. The trust account is
              subject to supervision by the Victorian Legal Services
              Board and is maintained in accordance with the Legal
              Profession Uniform General Rules 2015 (Rules) and the
              Uniform Law, effective at the time of the deposit. Any
              claim in relation to trust money must be made under
              Victorian law."
            ),
            tags$strong("Authority to draw on trust money"),
            tags$p(
              "You authorise us to draw on any money received into our
              trust account on your behalf to pay our legal fees as they
              become due and payable, in accordance with the Uniform
              Law and the Rules relating to the withdrawal of trust
              money for legal costs. You will be provided with a trust
              statement at the conclusion of your matter."
            ),
            tags$strong("File/storage destruction"),
            tags$p(
              "We will destroy our file seven years from concluding your
              matter. Before that time, you can make arrangements to
              collect it from us provided all money you owe us is paid in
              full. If your file is in electronic form, it will be returned to
              you in PDF form only."
            ),
            tags$strong("E-files"),
            tags$p(
              "You agree we may maintain your file, including bills, in
              electronic form, rather than maintaining a hard copy file.
              You acknowledge therefore that original documents held
              by us may not be available as hard copies."
            ),
            tags$p(
              "You acknowledge part or all the electronic file may be
              stored on servers maintained by third parties."
            ),
            tags$p(
              "If you require a copy of your file, or seek the file be
              provided to you on concluding the matter or terminating
              this agreement, we will provide you with any hard copy
              documents that are your property, and we will provide you
              with electronic copies of any of your documents held in
              electronic form. We will not be required to provide you
              with hard copies of your documents held electronically."
            ),
            tags$strong("Intellectual property"),
            tags$p(
              "Unless specifically agreed by us in writing to the contrary,
              we own the intellectual property rights (including copyright,
              patents, trademarks and other rights and the right to apply
              for the registration of such rights) in all work, documents
              and materials we create as a result of acting on your
              behalf."
            ),
            tags$p(
              "We grant you a non-exclusive, royalty free and worldwide
              licence to use any material we created and delivered to
              you in the course of providing you with our services."
            ),
            tags$p(
              "Generally, we do not assert the moral rights in works
              where we are the authors. We will notify you if we do
              require the recognition of our moral rights."
            ),
            tags$strong("Electronic communications"),
            tags$p(
              "Unless you specifically request us not to, we may
              communicate with you and provide documents, including
              bills, by email. We may also provide some services to you
              via online methods such as via websites. We are
              dependent on third parties such as telecommunications
              carriers to provide these services. While generally secure
              and reliable we cannot ensure the security or reliability of
              communications with you via the internet and you agree
              we are not required to do so."
            ),
            tags$strong("Changes by others to our work product"),
            tags$p(
              "Some of the work product will be provided to you in forms
              that can be changed after it leaves our control. We accept
              responsibility for the content of any material we provide
              directly to you. But we cannot and do not accept any
              responsibility for the accuracy, reliability, legal effect or
              lawfulness of any material changed by you or a third party.
              If you have any doubts about whether a document is
              changed or the effects of a change, please contact us."
            ),
            tags$strong("Ending this Costs Agreement"),
            tags$p(
              "This Costs Agreement may be ended at any time by our
              giving you at least 14 days notice of our intention to end
              the agreement or by you giving us notice in writing any
              time."
            ),
            tags$p(
              "We may stop providing services to you if you fail to pay
              our invoices or if you fail to provide us with adequate
              instructions."
            ),
            tags$p(
              "If this Costs Agreement is ended, you will be obliged to
              pay our professional fees and expenses incurred up until
              the agreement is formally ended."
            ),
            tags$strong("Lien over documents"),
            tags$p(
              "We are entitled to retain by lien, files, documents and
              other property in our possession or under our control as
              security for outstanding professional fees and expenses."
            )
          )
        )
      )
    )
  )
}

# Module Server -----------------------------------------------------------
mod_permitholder_server <- function(id, values, users){
  
  moduleServer(
    id,
    function(input, output, session){
      dataPostCodeMapping <- as.data.table(read.csv("inputs/australian_postcodes.csv"))
      dataCouncilInfo <- data.table(read.csv(path_input_council_info, fileEncoding = 'UTF-8-BOM'))
      #### END - Print selected information on the side  ----------------------------------
    }
  )
}