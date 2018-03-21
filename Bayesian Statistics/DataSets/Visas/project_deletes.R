
for (i in 1:length(visaFull)){
  if (visaFull$wage_offer_from_9089[i] <= 100){
    visaFull$pw_unit_of_pay_9089[i] == "Hour"
  } else if (visaFull$wage_offer_from_9089[i] > 100 & visaFull$wage_offer_from_9089[i] <=){
    visaFull$pw_unit_of_pay_9089[i] = visaFull$pw_unit_of_pay_9089[i] * 52
  } else if (visaFull$wage_offer_from_9089[i] == "Bi-Weekly"){
    visaFull$pw_unit_of_pay_9089[i] = visaFull$pw_unit_of_pay_9089[i] * 26
  } else if (visaFull$wage_offer_from_9089[i] == "Week"){
    visaFull$pw_unit_of_pay_9089[i] = visaFull$pw_unit_of_pay_9089[i] * 52
  } else if (visaFull$wage_offer_from_9089[i] == "Month"){
    visaFull$pw_unit_of_pay_9089[i] = visaFull$pw_unit_of_pay_9089[i] * 12
  } else if (visaFull$wage_offer_from_9089[i] == "Year") {
    visaFull$pw_unit_of_pay_9089[i] = visaFull$pw_unit_of_pay_9089[i]
  }
}





for (i in 1:length(visaFull)){
  if (visaFull$pw_unit_of_pay_9089[i] == "Hour"){
    visaFull$wage_offer_from_9089[i] = visaFull$wage_offer_from_9089[i] * 40 * 52
  } else if (visaFull$pw_unit_of_pay_9089[i] == "Week"){
    visaFull$wage_offer_from_9089[i] = visaFull$wage_offer_from_9089[i] * 52
  } else if (visaFull$pw_unit_of_pay_9089[i] == "Bi-Weekly"){
    visaFull$wage_offer_from_9089[i] = visaFull$wage_offer_from_9089[i] * 26
  } else if (visaFull$pw_unit_of_pay_9089[i] == "Week"){
    visaFull$wage_offer_from_9089[i] = visaFull$wage_offer_from_9089[i] * 52
  } else if (visaFull$pw_unit_of_pay_9089[i] == "Month"){
    visaFull$wage_offer_from_9089[i] = visaFull$wage_offer_from_9089[i] * 12
  } else if (visaFull$pw_unit_of_pay_9089[i] == "Year") {
    visaFull$wage_offer_from_9089[i] = visaFull$wage_offer_from_9089[i]
  }
}




index <- which(visaFull$pw_unit_of_pay_9089 == "hr")
visaFull[index, 'pw_unit_of_pay_9089'] <- "Hour"

index <- which(visaFull$pw_unit_of_pay_9089 == "mth")
visaFull[index, 'pw_unit_of_pay_9089'] <- "Month"

index <- which(visaFull$pw_unit_of_pay_9089 == "wk")
visaFull[index, 'pw_unit_of_pay_9089'] <- "Week"

index <- which(visaFull$pw_unit_of_pay_9089 == "bi")
visaFull[index, 'pw_unit_of_pay_9089'] <- "Bi-Weekly"
