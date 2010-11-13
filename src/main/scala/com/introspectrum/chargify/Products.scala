package com.introspectrum.chargify

import com.introspectrum.chargify.Preamble._
import org.apache.commons.httpclient._
import org.apache.commons.httpclient.methods._

case class ProductFamily(
	name: String,
	handle: String,
	accountingCode: String,
	description: String
)

case class Product(
	priceInCents: Int,
	name: String,
	handle: String,
	description: String,
	productFamily: ProductFamily,
	accountingCode: String,
	intervalUnit: IntervalUnit.Value,
	interval: Int
)

object IntervalUnit extends Enumeration {
	val Month = Value( "month")
	val Day = Value( "day")
}

trait Products {
	
	def getClient(): HttpClient

	def listProducts(): List[ Product] = {
		val method = new GetMethod( "/products.xml")
		method.setDoAuthentication( true)
		getClient().executeMethod( method)
		handleResponseCode( method)
		val list = parseReponse( method)
		( for ( node <- list\"product") yield nodeseq2product( node)) toList
	}

	def getProductById( id: Int): Option[ Product] = {
		val method = new GetMethod( "/products/" + id + ".xml")
		getClient().executeMethod( method)
		try {
			handleResponseCode( method)
			Some( parseReponse( method))
		} catch {
			case e: NotFoundException => None
		}
	}

	def getProductByHandle( handle: String): Option[ Product] = {
		val method = new GetMethod( "/products/handle/" + handle + ".xml")
		getClient().executeMethod( method)
		try {
			handleResponseCode( method)
			Some( parseReponse( method))
		} catch {
			case e: NotFoundException => None
		}
	}
}
