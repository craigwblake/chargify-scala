package com.introspectrum.chargify

import java.util.Date
import net.liftweb.util.HttpHelpers
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.auth._
import org.apache.commons.httpclient.methods._
import org.apache.commons.httpclient.protocol.Protocol
import scala.xml.{Node,NodeSeq}

case class Chargify( key: String, domain: String) extends Customers {

	val host = new HostConfiguration()
	host.setHost( domain + ".chargify.com", 443, Protocol.getProtocol( "https"))

	val client = new HttpClient()
	client.getParams().setAuthenticationPreemptive( true)
	client.getState().setCredentials( new AuthScope( domain + ".chargify.com", 443, "Chargify API"), new UsernamePasswordCredentials( key, "x"))
	client.setHostConfiguration( host)

	def getClient() = client
}
