package com.introspectrum.chargify

import com.introspectrum.chargify.Preamble._
import java.util.Date
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._

case class Subscription(
	id: Int,
	state: State.Value,
	balanceInCents: Int,
	currentPeriodStartedAt: Date,
	currentPeriodEndsAt: Date,
	trialStartedAt: Option[ Date],
	trialEndedAt: Option[ Date],
	activatedAt: Option[ Date],
	expiresAt: Option[ Date],
	createdAt: Date,
	updatedAt: Date,
	customer: Customer,
	product: Product,
	creditCard: Option[ StoredCreditCard],
	cancellationMessage: String
)

case class CreditCard(
	firstName: Option[ String],
	lastName: Option[ String],
	fullNumber: String,
	expirationMonth: Int,
	expirationYear: Int,
	cvv: Option[ Int],
	billingAddress: Option[ String],
	billingCity: Option[ String],
	billingState: Option[ String],
	billingZip: Option[ Int],
	billingCountry: Option[ String]
)

case class StoredCreditCard(
	cardType: Type.Value,
	expirationMonth: Int,
	expirationYear: Int,
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
	val SoftFailure = Value( "soft_failure")
	val PastDue = Value( "past_due")
	val Suspended = Value( "suspended")
	val Closed = Value( "closed")
	val Expired = Value( "expired")
	val Canceled = Value( "canceled")
}

trait Subscriptions {

	def getClient(): HttpClient

	def listSubscriptions( customer: Int): List[ Subscription] = {
		val method = new GetMethod( "/customers/" + customer + "/subscriptions.xml")
		getClient().executeMethod( method)
		handleResponseCode( method)
		val list = parseReponse( method)
		( for ( node <- list\"subscription") yield nodeseq2subscription( node)) toList
	}

	def getSubscription( id: Int): Option[ Subscription] = {
		val method = new GetMethod( "/subscriptions/" + id + ".xml")
		getClient().executeMethod( method)
		try {
			handleResponseCode( method)
			Some( parseReponse( method))
		} catch {
			case e: NotFoundException => None
		}
	}

	def deleteSubscription( id: Int, reason: Option[ String]) = {
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

	def createSubscription( product: Product, customer: Customer, creditCard: Option[ CreditCard]): Subscription = {
		val xml =
			<subscription>
				<product_handle>{product.handle}</product_handle>
				<customer_reference>{customer.reference}</customer_reference>
				{ creditCard match {
					case None =>
					case Some( creditCard) =>
						<credit_card_attributes>
							<full_number>{creditCard.fullNumber}</full_number>
							<expiration_month>{creditCard.expirationMonth}</expiration_month>
							<expiration_year>{creditCard.expirationYear}</expiration_year>
							{ emit( "first_name", creditCard.firstName)}
							{ emit( "last_name", creditCard.lastName)}
							{ emit( "cvv", creditCard.cvv)}
							{ emit( "billing_address", creditCard.billingAddress)}
							{ emit( "billing_city", creditCard.billingCity)}
							{ emit( "billing_state", creditCard.billingState)}
							{ emit( "billing_zip", creditCard.billingZip)}
							{ emit( "billing_country", creditCard.billingCountry)}
							{ emit( "last_name", creditCard.lastName)}
						</credit_card_attributes>
				}}
			</subscription>

		val method = new PostMethod( "/subscriptions.xml")
		method.setRequestEntity( new StringRequestEntity( xml, contentType, charset))
		getClient().executeMethod( method)
		handleResponseCode( method)
		parseReponse( method)
	}

	def editSubscription( id: Int, product: Option[ Product], creditCard: Option[ CreditCard]): Unit = {
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
							{ emit( "first_name", creditCard.firstName)}
							{ emit( "last_name", creditCard.lastName)}
							{ emit( "cvv", creditCard.cvv)}
							{ emit( "billing_address", creditCard.billingAddress)}
							{ emit( "billing_city", creditCard.billingCity)}
							{ emit( "billing_state", creditCard.billingState)}
							{ emit( "billing_zip", creditCard.billingZip)}
							{ emit( "billing_country", creditCard.billingCountry)}
							{ emit( "last_name", creditCard.lastName)}
						</credit_card_attributes>
				}}
			</subscription>

		val method = new PutMethod( "/subscriptions/" + id + ".xml")
		method.setRequestEntity( new StringRequestEntity( xml, contentType, charset))
		getClient().executeMethod( method)
		handleResponseCode( method)
		parseReponse( method)
	}
}
