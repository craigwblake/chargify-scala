package com.introspectrum.chargify

import java.util.Date
import net.liftweb.common.Full
import net.liftweb.util.PCDataXmlParser
import org.apache.commons.httpclient._
import scala.xml.{Node,NodeSeq}

object Preamble {

	val contentType = "application/xml"
	val charset = "UTF-8"

	implicit def node2string( value: Node) = value toString
	implicit def nodeseq2string( value: NodeSeq) = value.first.text
	implicit def nodeseq2date( value: NodeSeq) = DateTime.parse( value.first.text)
	implicit def nodeseq2int( value: NodeSeq) = Integer.parseInt( value.first.text)

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
			case 422 => throw new InvalidRequestException()
			case 500 => throw new InternalServerException()
			case _ =>
		}
	}

	case class ChargifyException( message: String) extends RuntimeException( message)
	case class AlreadyExistsException( m: String) extends ChargifyException( m)
	case class AuthenticationFailedException extends ChargifyException( "API authentication failed")
	case class DisabledEndpointException extends ChargifyException( "This endpoint is not enabled")
	case class NotFoundException extends ChargifyException( "The resource was not found")
	case class InvalidRequestException extends ChargifyException( "The request was malformed")
	case class InternalServerException extends ChargifyException( "An internal error occurred")
}
