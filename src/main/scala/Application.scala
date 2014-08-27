package com.hanhuy.android.tvm

import org.acra.ACRA
import org.acra.annotation.ReportsCrashes

/**
 * @author pfnguyen
 */
@ReportsCrashes(formKey = "",
  formUri = "http://hanhuy-acra.appspot.com/api/crashreport")
class Application extends android.app.Application {
  override def onCreate() {
    super.onCreate()
    Application._context = this
    ACRA.init(this)
  }
}

object Application {
  private var _context: Application = null
  def context = _context
}
