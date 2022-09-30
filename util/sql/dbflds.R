# Prepares the indices used to extract specific variables
# from the data. They are named with keys that represent 
# what kind of field they represent:
# - fkey: Foreign Keys
# - bool: Binary (binomial) variables coded as 1/0
# - field: Regular fields
set_variable_indices <- function(fkey = NULL, bool = NULL, field = NULL) {
  args <- list(fkey = fkey, bool = bool, field = field)
  custom.attr <- 'tag'
  
  keys <- purrr::imap(args, function(.x, .y) {
    if (is.null(.x))
      return()
    
    attr(.x, custom.attr) <- .y
    .x
  })
  
  spreadName <- function(x, y) {
    .f <- function(i) {
      if (!is.null(names(i)))
        return(i)
      
      name <- attr(i, which = custom.attr)
      setNames(i, rep(name, length(i)))
    }
    
    c(.f(x), .f(y))
  }
  
  Reduce(spreadName, keys, accumulate = FALSE)
}




get_selecion_args <- function() {
  tables <-
    c(
      "Interviewer",
      "Facility",
      "Health",
      "Legal",
      "Psychosocial",
      "Police",
      "Shelter",
      "Economic"
    )
  
  
  interviewer <- list('proj.name',
                      "",
                      c('interviewer', 'interviewer.contact'))
  
  facility <- list(
    c(
      "state",
      "lga",
      "interviewer",
      "device.id",
      "age",
      "showed.docs",
      "org.type",
      "how.data",
      "private.ques",
      "computer.secured",
      "contact.authority",
      "coc.signed",
      "refto.health",
      "refto.psych",
      "refto.police",
      "refto.legal",
      "refto.shelt",
      "refto.econ",
      "refto.other",
      "update.refdir",
      "choose.treatment"
    ),
    c(
      "uses.docs",
      "child.docs",
      "standard.forms",
      "data.is.stored",
      "priv",
      "priv.room",
      "serve.disabled",
      "disabled.special",
      "coc.copies",
      "coc.confidentiality",
      "coc.equity",
      "has.focalperson",
      "has.gbv.trained",
      "has.refdir",
      "choose.referral",
      "coordination"
    ),
    c(
      "start",
      "end",
      "today",
      "has.office",
      "has.phone",
      "continue",
      "consent",
      "orgname",
      "opstart",
      "gbvstart",
      "ward",
      "address",
      "phone",
      "email",
      "title",
      "info",
      "gps.long",
      "gps.lat",
      "gps.alt",
      "gps.prec",
      "oth.org.type",
      "open.247",
      "open.time",
      "close.time",
      "oth.gbv.dscrb",
      "staffname",
      "oth.fund.dscrb",
      "govt.spec",
      "fulltime.staff",
      "partime.staff",
      "female.staff",
      "doc.photo",
      "oth.docs.dscrb",
      "process.nodoc",
      "why.contact",
      "contact.case",
      "contact.authtype",
      "details.miss.equip",
      "oth.disabl.dscrb",
      "focalperson.contact",
      "num.gbv.trained",
      "who.gbv.trained",
      "which.gbv.trained",
      "refdir.pic",
      "oth.refto.dscrb",
      "gbvcase.contact",
      "why.nochoose.ref",
      "which.coord",
      "comment.coord",
      "service.othersdetail"
    )
  )
  
  health <- list(
    c("hf.type", "health.paid"),
    c("has.pep", "has.contracep", "access.srv", "forms.yes"),
    c(
      "hf.type.others",
      "oth.srvhealth.dscrb",
      "total.health",
      "has.no.pep",
      "has.no.contracep",
      "healthfee.clin",
      "healthfee.inj",
      "healthfee.pep",
      "healthfee.contra",
      "healthfee.hiv",
      "healthfee.sti",
      "healthfee.foren",
      "healthfee.psych",
      "healthfee.case",
      "healthfee.basic",
      "healthfee.other",
      "comment.elem",
      "comment.suppl",
      "oth.hlthtrain.dscrb",
      "qual.staff"
    )
  )
  
  legal <- list(
    c("legal.paid", "no.resources1"),
    "support.for.court",
    c(
      "oth.srvleg.dscrb",
      "total.legal",
      "legal.access",
      "legalfee.consult",
      "legalfee.rep",
      "legalfee.court",
      "legalfee.med",
      "legalfee.secur",
      "legalfee.counsel",
      "legalfee.other",
      "no.resources2"
    )
  )
  
  psych <- list(
    "psych.paid",
    "",
    c(
      "oth.srvpsy.dscrb",
      "total.psychosocial",
      "psych.access",
      "psychfee.counsel",
      "psychfee.case",
      "psychfee.therapy",
      "psychfee.safety",
      "psychfee.other",
      "oth.psychtrain.dscrb",
      "qualstaff.id"
    )
  )
  
  police <- list(
    "police.fees",
    c("gbv.police",
      "refer.otherpolice",
      "police.followup"),
    c(
      "who.gbvpolice",
      "oth.srvpol.dscrb",
      "trainedpolice.id",
      "total.police.rape",
      "total.police.ipv",
      "total.police.csa",
      "total.police.fgm",
      "total.police.oth",
      "oth.polnum.dscrb",
      "police.access",
      "policefee.case",
      "policefee.safety",
      "policefee.other",
      "oth.policeresrc.dscrb",
      "police.confidential"
    )
  )
  
  shelter <- list(
    "electricwater",
    c(
      "shelter.famfriendly",
      "shelter.kidfriendly",
      "shelter.support",
      "shelter.new.support"
    ),
    c(
      "health.srvshelt.dscrb",
      "oth.srvshelt.dscrb",
      "oth.sheltpriv.dscrb",
      "oth.sheltamen.dscrb",
      "total.shelter.f",
      "total.shelter.m"
    )
  )
  
  econ <- list("",
               c("econ.areas",
                 "econ.reject"),
               c("oth.srvecon.dscrb",
                 "total.economic"))
  
  all.groups <- list(interviewer,
                     facility,
                     health,
                     legal,
                     psych,
                     police,
                     shelter,
                     econ)
  
  var.index.list <-
    purrr::map(all.groups, function(x) {
      set_val <- function(i) {
        if (is.na(i))
          return()
        i
      }
      
      names(x) <- c("fkey", "bool", "field")
      set_variable_indices(
        fkey = set_val(x$fkey),
        bool = set_val(x$bool),
        field = set_val(x$field)
      )
    })
  
  tibble::tibble(table = tables, var.index = var.index.list)
}
