package org.hfgiii.drools

import java.math.BigDecimal

import droolsbook.Account
import org.joda.time.LocalDate
import org.specs2.mutable.Specification


trait TestDecisionTables extends Specification with DroolsContext {

  private val DATE            = new LocalDate(2008, 1, 1).toDateTimeAtStartOfDay

  def insertAccountAndRunRules(account:Account) {
    buildKnowledge
    newKnowledgeSession

    insertFacts(account)

    runRules ()
  }

   sequential

   "Deposit Amount for a Number of Days" should {
     "deposit 125 Euros for 400 Days" in {
       val account = new Account
       account.setType(Account.Type.SAVINGS)
       account.setBalance(new BigDecimal("125.00"))
       account.setCurrency("EUR")
       account.setStartDate(DATE.minusDays(40))
       account.setEndDate(DATE)

       insertAccountAndRunRules(account)

       new BigDecimal("3.00") === account.getInterestRate

     }

     "deposit 20 USD for 10 days" in {
       val account = new Account
       account.setType(Account.Type.SAVINGS)
       account.setBalance(new BigDecimal("20.00"))
       account.setCurrency("USD")
       account.setStartDate(DATE)
       account.setEndDate(DATE.plusDays(10))

       insertAccountAndRunRules(account)

       BigDecimal.ZERO.setScale(2) === account.getInterestRate

     }

     "deposit 9000 EUROS for 11 months " in {
       val account = new Account

       account.setType(Account.Type.SAVINGS)
       account.setBalance(new BigDecimal("9000.00"))
       account.setCurrency("EUR")
       account.setStartDate(DATE.minusMonths(11))
       account.setEndDate(DATE)

       insertAccountAndRunRules(account)

       new BigDecimal("3.75") === account.getInterestRate

     }
   }

   "Special deposits " should {
       "No interest Rate" in {
         val account = new Account
         account.setType(Account.Type.SAVINGS)
         account.setBalance(new BigDecimal("1000000.00"))
         account.setCurrency("EUR")
         account.setStartDate(DATE.minusMonths(12))
         account.setEndDate(DATE)

         insertAccountAndRunRules(account)

         account.getInterestRate === null
       }

       "Default Transaction Rate " in {
         val account = new Account
         account.setType(Account.Type.TRANSACTIONAL)
         account.setCurrency("EUR")

         insertAccountAndRunRules(account)

         new BigDecimal("0.01") must_== account.getInterestRate

       }

       "Default Student Rate " in {
         val account = new Account
         account.setType(Account.Type.STUDENT)
         account.setBalance(new BigDecimal("150.00"))
         account.setCurrency("EUR")

         insertAccountAndRunRules(account)

         new BigDecimal("1.00") must_== account.getInterestRate
       }
   }

}