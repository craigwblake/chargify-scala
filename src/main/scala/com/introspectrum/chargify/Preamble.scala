package com.introspectrum.chargify

import java.util.Date
import net.liftweb.common.Full
import net.liftweb.util.PCDataXmlParser
import org.apache.commons.httpclient._
import scala.xml._

case class Errors( errors: List[ String])

case class ChargifyException( message: String) extends RuntimeException( message)
case class AlreadyExistsException( m: String) extends ChargifyException( m)
case class AuthenticationFailedException extends ChargifyException( "API authentication failed")
case class DisabledEndpointException extends ChargifyException( "This endpoint is not enabled")
case class NotFoundException extends ChargifyException( "The resource was not found")
case class InvalidRequestException( errors: Errors) extends ChargifyException( "The request was invalid: " + errors)
case class InternalServerException extends ChargifyException( "An internal error occurred")

object Preamble {

	val contentType = "application/xml"
	val charset = "UTF-8"

	implicit def node2string( value: Node) = value toString
	implicit def nodeseq2string( value: NodeSeq) = value.first.text
	implicit def nodeseq2date( value: NodeSeq) = DateTime.parse( value.first.text)
	implicit def nodeseq2int( value: NodeSeq) = Integer.parseInt( value.first.text)
	
	implicit def nodeseq2dateoption( value: NodeSeq) = value \ "@nil" match {
		case Text( "true") => Some( DateTime.parse( value.first.text))
		case _ => None
	}

	implicit def nodeseq2productfamily( r: NodeSeq): ProductFamily = {
		ProductFamily( r\"name", r\"handle", r\"accounting_code", r\"description")
	}

	implicit def nodeseq2product( r: NodeSeq): Product = {
		Product( r\"price_in_cents", r\"name", r\"handle", r\"description", r\"product_family", r\"accounting_code", r\"interval_unit", r\"interval")
	}

	implicit def nodeseq2intervalunit( r: NodeSeq): IntervalUnit.Value = IntervalUnit.valueOf( r text) match {
		case Some( value) => value
		case None => throw new RuntimeException( "Invalid enumeration value for IntervalUnit: " + ( r text))
	}

	implicit def nodeseq2customer( r: NodeSeq): Customer = {
		Customer( r\"id", r\"first_name", r\"last_name", r\"email", r\"organization", r\"reference", r\"created_at", r\"updated_at")
	}

	implicit def nodeseq2type( r: NodeSeq): Type.Value = Type.valueOf( r text) match {
		case Some( value) => value
		case None => throw new RuntimeException( "Invalid enumeration value for Type: " + ( r text))
	}

	implicit def nodeseq2state( r: NodeSeq): State.Value = State.valueOf( r text) match {
		case Some( value) => value
		case None => throw new RuntimeException( "Invalid enumeration value for State: " + ( r text))
	}

	implicit def nodeseq2creditcardoption( r: NodeSeq): Option[ StoredCreditCard] = r match {
		case NodeSeq.Empty => None
		case r => Some( StoredCreditCard( r\"type", r\"expiration_month", r\"expiration_year", r\"first_name", r\"last_name", r\"masked_card_number"))
	}

	implicit def nodeseq2subscription( r: NodeSeq): Subscription = {
		Subscription( r\"id", r\"state", r\"balance_in_cents", r\"current_period_started_at", r\"current_period_ends_at", r\"trial_started_at", r\"trial_ended_at", r\"activated_at", r\"expires_at", r\"created_at", r\"updated_at", r\"customer", r\"product", r\"credit_card", r\"cancellation_message")
	}

	def none = None

	def emit( node: String, value: Option[ Any]): NodeSeq = {
		value match {
			case None => NodeSeq.Empty
			case Some( x) => Elem( null, node, null, TopScope, Text( String.valueOf( x)))
		}
	}

	def parseReponse( method: HttpMethodBase): Node = {
		PCDataXmlParser( method.getResponseBodyAsString()) match {
			case Full( x) => x( 0)
			case _ => throw new RuntimeException( "Could not parse response")
		}
	}

	def handleResponseCode( method: HttpMethodBase) = {
		val line = method.getStatusLine()
		line.getStatusCode() match {
			case 401 => throw new AuthenticationFailedException()
			case 403 => throw new DisabledEndpointException()
			case 404 => throw new NotFoundException()
			case 500 => throw new InternalServerException()
			case 422 =>
				val errors: List[ String] = parseReponse( method) match {
					case <errors>{ errors @ _*}</errors> =>
						(for ( error @ <error>{_}</error> <- errors) yield error text).toList
					case _ => List()
				}
				throw new InvalidRequestException( Errors( errors))
			case _ =>
		}
	}
}
