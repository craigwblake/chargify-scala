package com.introspectrum.chargify

import com.introspectrum.chargify.Preamble._
import java.util.Date
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._

case class Subscription(
	id: int,
	state: State.Value,
	balanceInCents: int,
	currentPeriodStartedAt: Date,
	currentPeriodEndsAt: Date,
	trialStartedAt: Date,
	trialEndedAt: Date,
	activateAt: Date,
	expiresAt: Date,
	createdAt: Date,
	updatedAt: Date,
	customer: Customer,
	product: Product,
	creditCard: StoredCreditCard,
	cancellationMessage: String
)

case class CreditCard(
	firstName: Option[ String],
	lastName: Option[ String],
	fullNumber: String,
	expirationMonth: int,
	expirationYear: int,
	cvv: Option[ int],
	billingAddress: Option[ String],
	billingCity: Option[ String],
	billingState: Option[ String],
	billingZip: Option[ int],
	billingCountry: Option[ String]
)

case class StoredCreditCard(
	cardType: Type.Value,
	expirationMonth: int,
	expirationYear: int,
	firstName: String,
	lastName: String,
	maskedCardNumber: String
)

object Type extends Enumeration {
	val Visa = Value( "visa")
	val Master = Value( "master")
	val Bogus = Value( "bogus")
}

object State extends Enumeration {
	val Trialing = Value( "trialing")
	val Active = Value( "active")
	val Soft_failure = Value( "soft_failure")
	val PastDue = Value( "past_due")
	val Suspended = Value( "suspended")
	val Closed = Value( "closed")
	val Expired = Value( "expired")
}

trait Subscriptions {

	def getClient(): HttpClient

	def listSubscriptions( customer: int): List[ Subscription] = {
		val method = new GetMethod( "/customers/" + customer + "/subscriptions.xml")
		getClient().executeMethod( method)
		handleResponseCode( method)
		val list = parseReponse( method)
		( for ( node <- list\"subscription") yield nodeseq2subscription( node)) toList
	}

	def getSubscription( id: int): Option[ Subscription] = {
		val method = new GetMethod( "/subscriptions/" + id + ".xml")
		getClient().executeMethod( method)
		try {
			handleResponseCode( method)
			Some( parseReponse( method))
		} catch {
			case e: NotFoundException => None
		}
	}

	def deleteSubscription( id: String, reason: Option[ String]) = {
		val method = new DeleteMethod( "/subscriptions/" + id + ".xml")

		// HttpClient does not support request entities with the DELETE operation
		/*
		reason match {
			case None =>
			case Some( reason) =>
				val xml =
					<subscription>
						<cancellation_message>{reason}</cancellation_message>
					</subscription>
				method.setRequestEntity( new StringRequestEntity( xml, contentType, charset))
		}
		*/

		getClient().executeMethod( method)
		handleResponseCode( method)
	}

	def createSubscription( product: Product, customer: Customer, creditCard: CreditCard): Subscription = {
		val xml =
			<subscription>
				<product_handle>{product.handle}</product_handle>
				<customer_reference>{customer.id}</customer_reference>
				<credit_card_attributes>
					<full_number>{creditCard.fullNumber}</full_number>
					<expiration_month>{creditCard.expirationMonth}</expiration_month>
					<expiration_year>{creditCard.expirationYear}</expiration_year>
					{ creditCard.firstName match {
						case None =>
						case Some( firstName) => <first_name>{firstName}</first_name>
					}}
					{ creditCard.lastName match {
						case None =>
						case Some( lastName) => <last_name>{lastName}</last_name>
					}}
					{ creditCard.cvv match {
						case None =>
						case Some( cvv) => <cvv>{cvv}</cvv>
					}}
					{ creditCard.billingAddress match {
						case None =>
						case Some( billingAddress) => <billing_address>{billingAddress}</billing_address>
					}}
					{ creditCard.billingCity match {
						case None =>
						case Some( billingCity) => <billing_city>{billingCity}</billing_city>
					}}
					{ creditCard.billingState match {
						case None =>
						case Some( billingState) => <billing_state>{billingState}</billing_state>
					}}
					{ creditCard.billingZip match {
						case None =>
						case Some( billingZip) => <billing_zip>{billingZip}</billing_zip>
					}}
					{ creditCard.billingCountry match {
						case None =>
						case Some( billingCountry) => <billing_country>{billingCountry}</billing_country>
					}}
				</credit_card_attributes>
			</subscription>

		val method = new PostMethod( "/subscriptions.xml")
		method.setRequestEntity( new StringRequestEntity( xml, contentType, charset))
		getClient().executeMethod( method)
		handleResponseCode( method)
		parseReponse( method)
	}

	def editSubscription( id: int, product: Option[ Product], creditCard: Option[ CreditCard]): Unit = {
		val xml =
			<subscription>
				{ product match {
					case None =>
					case Some( product) => <product_handle>{product.handle}</product_handle>
				}}
				{ creditCard match {
					case None =>
					case Some( creditCard) =>
						<credit_card_attributes>
							<full_number>{creditCard.fullNumber}</full_number>
							<expiration_month>{creditCard.expirationMonth}</expiration_month>
							<expiration_year>{creditCard.expirationYear}</expiration_year>
							{ creditCard.firstName match {
								case None =>
								case Some( firstName) => <first_name>{firstName}</first_name>
							}}
							{ creditCard.lastName match {
								case None =>
								case Some( lastName) => <last_name>{lastName}</last_name>
							}}
							{ creditCard.cvv match {
								case None =>
								case Some( cvv) => <cvv>{cvv}</cvv>
							}}
							{ creditCard.billingAddress match {
								case None =>
								case Some( billingAddress) => <billing_address>{billingAddress}</billing_address>
							}}
							{ creditCard.billingCity match {
								case None =>
								case Some( billingCity) => <billing_city>{billingCity}</billing_city>
							}}
							{ creditCard.billingState match {
								case None =>
								case Some( billingState) => <billing_state>{billingState}</billing_state>
							}}
							{ creditCard.billingZip match {
								case None =>
								case Some( billingZip) => <billing_zip>{billingZip}</billing_zip>
							}}
							{ creditCard.billingCountry match {
								case None =>
								case Some( billingCountry) => <billing_country>{billingCountry}</billing_country>
							}}
						</credit_card_attributes>
				}}
			</subscription>

		val method = new PutMethod( "/customers/" + id + ".xml")
		method.setRequestEntity( new StringRequestEntity( xml, contentType, charset))
		getClient().executeMethod( method)
		handleResponseCode( method)
		parseReponse( method)
	}
}
