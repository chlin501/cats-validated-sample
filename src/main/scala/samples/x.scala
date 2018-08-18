package samples

import cats.data.Validated 
import cats.data.ValidatedNel
import cats.syntax._
import cats.implicits._

final case class Registered(username: String, password: String) 

trait ValidationError {

  def exception(): Exception  

}

case class InvalidUsername(ex: Throwable) extends ValidationError {

  override def exception(): Exception = ex.asInstanceOf[Exception]

}

case class InvalidPassword(ex: Throwable) extends ValidationError {

  override def exception(): Exception = ex.asInstanceOf[Exception]

}

object Validator {

  type ValidationResult[A] = ValidatedNel[ValidationError, A]

  def validateUsername(username: String): ValidationResult[String] = 
    if(username.matches("^[a-zA-Z0-9]+$")) username.validNel 
    else InvalidUsername(new RuntimeException(s"Invalid username: $username")).invalidNel

  def validatePassword(password: String): ValidationResult[String] = 
    if(password.matches("(?=^.{10,}$)((?=.*\\d)|(?=.*\\W+))(?![.\\n])(?=.*[A-Z])(?=.*[a-z]).*$")) password.validNel 
    else InvalidPassword(new RuntimeException(s"Invalid password: $password")).invalidNel

  def validate(
    username: String, password: String
  ): ValidationResult[Registered] = {
    (validateUsername(username), validatePassword(password)).mapN(Registered)
  }

}


object x {

  // See https://typelevel.org/cats/datatypes/validated.html
  def main(args: Array[String]) {
    println(Validator.validate("Joe%%%", "password")) // invalid
    println(Validator.validate("Joe", "Passw0r$123")) // valid
   
  }

}
