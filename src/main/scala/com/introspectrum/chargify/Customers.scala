package com.introspectrum.chargify

import com.introspectrum.chargify.Preamble._
import java.util.Date
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._
import scala.xml.{Node,NodeSeq}

trait Customers {

	def getClient(): HttpClient

	implicit def nodeseq2customer( r: NodeSeq): Customer = {
		Customer( r\"id", r\"first_name", r\"last_name", r\"email", r\"organization", r\"reference", r\"created_at", r\"updated_at")
	}

	def listCustomers(): List[ Customer] = {
		val method = new GetMethod( "/customers.xml")
		method.setDoAuthentication( true)
		getClient().executeMethod( method)
		handleResponseCode( method)
		val list = parseReponse( method)
		( for ( node <- list\"customer") yield nodeseq2customer( node)) toList
	}

	def getCustomerById( id: String): Option[ Customer] = {
		val method = new GetMethod( "/customers/" + id + ".xml")
		getClient().executeMethod( method)
		try {
			handleResponseCode( method)
			Some( parseReponse( method))
		} catch {
			case e: NotFoundException => None
		}
	}

	def getCustomerByReference( reference: String): Option[ Customer] = {
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

	def createCustomer( firstName: String, lastName: String, email: String, reference: Option[ String]): Customer = {
		/*
		getCustomer( email) match {
			case Some( x) => throw new AlreadyExistsException( "Customer already exists")
			case None =>
		}
		*/

		val xml =
			<customer>
				<email>{email}</email>
				<first_name>{firstName}</first_name>
				<last_name>{lastName}</last_name>
				{ reference match {
						case Some( reference) => <reference>{reference}</reference>
						case None =>
				}}
			</customer>

		val method = new PostMethod( "/customers.xml")
		method.setRequestEntity( new StringRequestEntity( xml, contentType, charset))
		getClient().executeMethod( method)
		handleResponseCode( method)
		parseReponse( method)
	}

	def editCustomer( id: Int, firstName: Option[ String], lastName: Option[ String], email: Option[ String], reference: Option[ String]): Customer = {
		val xml =
			<customer>
				{ email match {
						case Some( email) => <email>{email}</email>
						case None =>
				}}
				{ firstName match {
						case Some( firstName) => <first_name>{firstName}</first_name>
						case None =>
				}}
				{ lastName match {
						case Some( lastName) => <last_name>{lastName}</last_name>
						case None =>
				}}
				{ reference match {
						case Some( reference) => <reference>{reference}</reference>
						case None =>
				}}
			</customer>

		val method = new PutMethod( "/customers/" + id + ".xml")
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
