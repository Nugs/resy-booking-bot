package com.resy

import org.mockito.Mockito._
import org.mockito.ArgumentMatchers._
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import java.time.{Clock, Instant, LocalDate, ZoneId}
import java.util.TimeZone
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class BookReservationWorkflowTest extends AsyncWordSpec with Matchers with Eventually {

  private val clock = Clock.fixed(Instant.now(), ZoneId.of("GMT"))

  private val venue = Venue(
    id                      = "abcdef",
    hourOfDayToStartBooking = 10,
    advance                 = 10.days,
    diningTypes             = List.empty,
    info                    = None,
    timeZone                = TimeZone.getTimeZone("GMT")
  )

  private val details = BookingDetails(
    authToken      = "12345",
    apiKey         = "67890",
    venue          = venue,
    date           = LocalDate.now(clock),
    preferences    = List.empty,
    partySize      = 2,
    retryTimeout   = 5.seconds,
    wakeAdjustment = 5.seconds
  )
  private val mockApi: ResyApiWrapper = mock(classOf[ResyApiWrapper])
  private val underTest               = new BookReservationWorkflow(mockApi)(details)

  "testGetLeadTime should invoke the API and use the value from the API over config" in {
    val leadTime = 5.days
    when(mockApi.execute(any[ApiDetails], any[Map[String, String]])(any[BookingDetails]))
      .thenReturn(Future.successful(s"""{"lead_time_in_days": ${leadTime.toDays}}"""))

    eventually {
      underTest.getLeadTime.map(_ shouldBe leadTime)
    }
  }

//  "testRetryFindReservation" in {}
//
//  "testGetReservationDetails" in {}
//
//  "testBookReservation" in {}

}
