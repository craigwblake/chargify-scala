package com.introspectrum.chargify

import com.introspectrum.chargify.Preamble._
import java.util.Date
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._
import scala.xml.{Node,NodeSeq}

trait Customers {

	def getClient(): HttpClient

	implicit def nodeseq2customer( r: NodeSeq): Customer = {
		// More flexible XML structure?
		//Customer( r\"id", r\"first_name", r\"last_name", r\"email", r\"organization", r\"reference", r\"createdAt", r\"updatedAt")
		r match {
			case <customer>
					<id>{id}</id>
					<first_name>{firstName}</first_name>
					<last_name>{lastName}</last_name>
					<email>{email}</email>
					<organization>{organization}</organization>
					<reference>{reference}</reference>
					<createdAt>{createdAt}</createdAt>
					<updatedAt>{updatedAt}</updatedAt>
				</customer>
				=> Customer( id, firstName, lastName, email, organization, reference, createdAt, updatedAt)
			case x => throw new RuntimeException( "Failed matching customer record: " + x)
		}
	}

	def listCustomers(): List[ Customer] = {
		val method = new GetMethod( "/customers.xml")
		method.setDoAuthentication( true)
		getClient().executeMethod( method)
		handleResponseCode( method)
		val list = parseReponse( method)
		( for ( node <- list\"customer") yield nodeseq2customer( node)) toList
	}

	def getCustomer( reference: String): Option[ Customer] = {
		val method = new GetMethod( "/customers/lookup.xml?reference=" + reference)
		getClient().executeMethod( method)
		try {
			handleResponseCode( method)
			Some( parseReponse( method))
		} catch {
			case e: NotFoundException => None
		}
	}

	def deleteCustomer( id: String) = {
		val method = new DeleteMethod( "/customers/" + id + ".xml")
		getClient().executeMethod( method)
		handleResponseCode( method)
		Some( parseReponse( method))
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

		val method = new PostMethod( "/customers.xml")
		method.setRequestEntity( new StringRequestEntity( xml, contentType, charset))
		getClient().executeMethod( method)
		handleResponseCode( method)
		parseReponse( method)
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
}
