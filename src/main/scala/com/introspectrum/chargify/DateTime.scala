package com.introspectrum.chargify

import java.text.SimpleDateFormat
import java.util.Date
import java.util.TimeZone

object DateTime {

	val Pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"

	def parse( date: String): Date = {
		val format = new SimpleDateFormat( Pattern)
		val builder = new StringBuilder( date)
		builder.deleteCharAt( builder.length() - 3)
		format.parse( builder.toString)
	}

	def format( date: Date): String = format( date, "GMT")

	def format( date: Date, timeZone: String): String = {
		val format = new SimpleDateFormat( Pattern)
		format.setTimeZone( TimeZone.getTimeZone( timeZone))
		val builder = new StringBuilder( format.format( date))
		builder.insert( builder.length() - 2, ':').toString
	}
}
