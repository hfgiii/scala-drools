package droolsbook;

import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.joda.time.DateTime;
import org.joda.time.Months;

import java.io.Serializable;
import java.math.BigDecimal;

/**
 * @author miba
 * 
 */
public class Account implements Serializable {

  public enum Type {
    TRANSACTIONAL, STUDENT, SAVINGS, JOINT
  }

  private String name;
  private Long number;
  private Type type = Type.TRANSACTIONAL;
  private BigDecimal balance = BigDecimal.ZERO;
  private BigDecimal interestRate;
  private String currency;
  private Customer owner;
  private DateTime startDate;
  private DateTime endDate;
  private String uuid;

  public int getMonthsBetweenStartAndEndDate() {
    if (startDate == null || endDate == null) {
      return 0;
    }
    return Months.monthsBetween(startDate, endDate)
        .getMonths();
  }

  public DateTime getStartDate() {
    return startDate;
  }

  public void setStartDate(DateTime startDate) {
    this.startDate = startDate;
  }

  public DateTime getEndDate() {
    return endDate;
  }

  public void setEndDate(DateTime endDate) {
    this.endDate = endDate;
  }

  public String getUuid() {
    return uuid;
  }
  
  public void setUuid(String uuid) {
    this.uuid = uuid;
  }
  
  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public Long getNumber() {
    return number;
  }

  public void setNumber(Long number) {
    this.number = number;
  }

  public BigDecimal getBalance() {
    return balance;
  }

  public void setBalance(BigDecimal balance) {
    if (balance == null) {
      throw new IllegalArgumentException(
          "droolsbook.Account balance cannot be null.");
    }
    this.balance = balance;
  }

  public BigDecimal getInterestRate() {
    return interestRate;
  }

  public void setInterestRate(BigDecimal interestRate) {
    this.interestRate = interestRate;
  }

  public Type getType() {
    return type;
  }

  public void setType(Type type) {
    this.type = type;
  }

  public String getCurrency() {
    return currency;
  }

  public void setCurrency(String currency) {
    this.currency = currency;
  }

  public Customer getOwner() {
    return owner;
  }

  public void setOwner(Customer owner) {
    this.owner = owner;
  }

  @Override
  public boolean equals(final Object other) {
    if (this == other)
      return true;
    if (!(other instanceof Account))
      return false;
    Account castOther = (Account) other;
    return new EqualsBuilder().append(name, castOther.name)
        .append(number, castOther.number).append(type,
            castOther.type).append(balance, castOther.balance)
        .append(interestRate, castOther.interestRate).append(
            currency, castOther.currency).append(startDate,
            castOther.startDate).append(endDate,
            castOther.endDate).isEquals();
  }

  @Override
  public int hashCode() {
    return new HashCodeBuilder(-806458195, -1310850617)
        .append(name).append(number).append(type).append(
            balance).append(interestRate).append(currency)
        .append(startDate).append(endDate).toHashCode();
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("name", name)
        .append("number", number).append("type", type).append(
            "balance", balance).append("interestRate",
            interestRate).append("currency", currency).append(
            "startDate", startDate).append("endDate", endDate)
        .toString();
  }

}
