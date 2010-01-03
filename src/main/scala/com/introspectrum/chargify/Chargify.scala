package com.introspectrum.chargify

import java.util.Date
import net.liftweb.common.Full
import net.liftweb.util.HttpHelpers
import net.liftweb.util.PCDataXmlParser
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.auth._
import org.apache.commons.httpclient.methods._
import scala.xml.{Node,NodeSeq}

case class Chargify( key: String, domain: String) {

	implicit def node2string( value: Node) = value toString
	implicit def nodeseq2string( value: NodeSeq) = value.first.text
	implicit def nodeseq2date( value: NodeSeq) = DateTime.parse( value.first.text)
	implicit def nodeseq2int( value: NodeSeq) = Integer.parseInt( value.first.text)
	
	implicit def nodeseq2customer( r: NodeSeq): Customer = {
		Customer( r\"id", r\"first_name", r\"last_name", r\"email", r\"organization", r\"reference", r\"createdAt", r\"updatedAt")
	}

	val client = new HttpClient()
	client.getParams().setAuthenticationPreemptive( true)
	client.getState().setCredentials( new AuthScope( domain + ".chargify.com", 443, AuthScope.ANY_REALM), new UsernamePasswordCredentials( key, "x"))

	val contentType = "application/xml"
	val charset = "UTF-8"
	val host = "https://" + domain + ".chargify.com/"

	def listCustomers(): List[ Customer] = {
		val method = new GetMethod( host + "customers.xml")
		method.setDoAuthentication( true)
		client.executeMethod( method)
		handleResponseCode( method)
		val list = parseReponse( method)
		( for ( node <- list\"customer") yield nodeseq2customer( node)) toList
	}

	def getCustomer( reference: String): Option[ Customer] = {
		val method = new GetMethod( host + "customers/lookup.xml?reference=" + reference)
		client.executeMethod( method)
		try {
			handleResponseCode( method)
			Some( parseReponse( method))
		} catch {
			case e: NotFoundException => None
		}
	}

	def createCustomer( firstName: String, lastName: String, email: String): Customer = {
		getCustomer( email) match {
			case Some( x) => throw new AlreadyExistsException( "Customer already exists")
			case None =>
		}

		val xml =
			<customer>
				<email>{email}</email>
				<first_name>{firstName}</first_name>
				<last_name>{lastName}</last_name>
			</customer>

		val method = new PostMethod( host + "customers.xml")
		method.setRequestEntity( new StringRequestEntity( xml, contentType, charset))
		client.executeMethod( method)
		handleResponseCode( method)
		parseReponse( method)
	}

	def parseReponse( method: HttpMethodBase): Node = {
		PCDataXmlParser( method.getResponseBodyAsString()) match {
			case Full( x) => x( 0)
			case _ => throw new RuntimeException( "Could not parse script for test")
		}
	}

	def handleResponseCode( method: HttpMethodBase) = {
		val line = method.getStatusLine()
		line.getStatusCode() match {
			case 401 => throw new AuthenticationFailedException()
			case 403 => throw new DisabledEndpointException()
			case 404 => throw new NotFoundException()
			case 422 => throw new InvalidRequestException()
			case 500 => throw new InternalServerException()
			case _ =>
		}
	}

	case class Customer(
		id: Int,
		firstName: String,
		lastName: String,
		email: String,
		organization: String,
		reference: String,
		createdAt: Date,
		updatedAt: Date)

	case class ChargifyException( message: String) extends RuntimeException( message)
	case class AlreadyExistsException( m: String) extends ChargifyException( m)
	case class AuthenticationFailedException extends ChargifyException( "API authentication failed")
	case class DisabledEndpointException extends ChargifyException( "This endpoint is not enabled")
	case class NotFoundException extends ChargifyException( "The resource was not found")
	case class InvalidRequestException extends ChargifyException( "The request was malformed")
	case class InternalServerException extends ChargifyException( "An internal error occurred")
}
