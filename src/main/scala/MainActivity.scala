package com.hanhuy.android.tvm

import android.app.Activity
import android.content.Context
import android.graphics._
import android.graphics.drawable.Drawable
import android.os.Bundle
import android.support.v4.view.{ViewPager, PagerAdapter}
import android.text.TextUtils.TruncateAt
import android.text._
import android.view.View.OnTouchListener
import android.view._
import android.widget.AbsListView.OnScrollListener
import android.widget._
import macroid._
import macroid.FullDsl._

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Try

object MainActivity {
  sealed trait FieldType
  case object IntegerField extends FieldType
  case object DecimalField extends FieldType
  case object DollarField  extends FieldType

  case class AmortRowData(n: Int, ipaid: Double, pvpaid: Double,
                          pvremain: Double, iTotal: Double, equity: Double)

  case class AmortRow(ipaid: TextView, pvpaid: TextView,
                      pvremain: TextView, iTotal: TextView, equity: TextView)
  def tweak[A <: View,B](f: A => B) = Tweak[A](f(_))
  private lazy val primitiveMap: Map[Class[_],Class[_]] = Map(
    classOf[java.lang.Integer]   -> java.lang.Integer.TYPE,
    classOf[java.lang.Double]    -> java.lang.Double.TYPE,
    classOf[java.lang.Short]     -> java.lang.Short.TYPE,
    classOf[java.lang.Float]     -> java.lang.Float.TYPE,
    classOf[java.lang.Character] -> java.lang.Character.TYPE,
    classOf[java.lang.Byte]      -> java.lang.Byte.TYPE
  )

  val prefs = Application.context.getSharedPreferences(
    "userinput", Context.MODE_PRIVATE)

  case class TVMValues(pv: Option[BigDecimal], fv: Option[BigDecimal],
                       i: Option[BigDecimal], a: Option[BigDecimal],
                       n: Option[Int], nyr: Int) {
    val editor = prefs.edit()
    setPref(pv map (_.setScale(2, BigDecimal.RoundingMode.HALF_UP)), "pv")
    setPref(fv map (_.setScale(2, BigDecimal.RoundingMode.HALF_UP)), "fv")
    setPref(i map  (_.setScale(3, BigDecimal.RoundingMode.HALF_UP)),  "i")
    setPref(a map  (_.setScale(2, BigDecimal.RoundingMode.HALF_UP)),  "a")
    setPref(n,  "n")
    editor.putString("nyr", nyr.toString)
    editor.apply()

    private def setPref[A](value: Option[A], key: String) {
      value foreach { v =>
        editor.putString(key, v.toString)
      }
    }
  }

  abstract class LpRelation[V <: ViewGroup, LP <: ViewGroup.LayoutParams : ClassTag] {
    def lpType = implicitly[ClassTag[LP]].runtimeClass
    def lp(args: Any*) = lpType.getConstructor(
      args map { a =>
        val c = a.getClass
        primitiveMap.getOrElse(c, c)
      }:_*).newInstance(args map (_.asInstanceOf[AnyRef]): _*).asInstanceOf[LP]
  }
  implicit object LLRelation extends LpRelation[LinearLayout, LinearLayout.LayoutParams]
  implicit object TRRelation extends LpRelation[TableRow, TableRow.LayoutParams]

  def lp2[V <: ViewGroup,LP <: ViewGroup.LayoutParams, C](args: Any*)
                                                         (p: LP => C)
                                                         (implicit r: LpRelation[V,LP]) = tweak {
    v: View =>
      val lp = r.lp(args: _*)
      p(lp)
      v.setLayoutParams(lp)
  }
}
class MainActivity extends Activity with Contexts[Activity] with AutoLogTag with IdGeneration {
  import MainActivity._
  import ViewGroup.LayoutParams._

  def tvToBD(f: TextView) = {
    import scala.util.control.Exception.catching
    catching(classOf[NumberFormatException]) opt {
      BigDecimal(f.getText.toString)
    }
  }
  def loadDefaults() {
    def loadPref[A <: TextView](view: A, key: String, default: String) {
      view.setText(prefs.getString(key, default))
    }

    loadPref(pvField,  "pv",  "")
    loadPref(fvField,  "fv",  "0")
    loadPref(iField,   "i",   "4.500")
    loadPref(aField,   "a",   "")
    loadPref(nField,   "n",   "360")
    loadPref(nyrField, "nyr", "12")
  }
  def values = {
    TVMValues(
      tvToBD(pvField),
      tvToBD(fvField),
      tvToBD(iField),
      tvToBD(aField),
      Try(nField.getText.toString.toInt).toOption,
      nyrField.getText.toString.toInt)
  }

  def periodRate(values: TVMValues) = values.i map { _ / 100 / values.nyr }
  def periodRate(i: BigDecimal, nyr: Int) = i / 100 / nyr

  private var popupWindow   = Option.empty[PopupWindow]
  private var inputText: EditText = _
  private var decimalButton: Button = _
  private var selectedField = Option.empty[TextView]
  private var viewPager: ViewPager = _
  private var amortPageButton: View = _

  private var pvField: TextView = _
  private var fvField: TextView = _
  private var iField: TextView = _
  private var aField: TextView = _
  private var nField: TextView = _
  private var nyrField: TextView = _

  val deleteLast = tweak { tv: EditText =>
    val sel = tv.getSelectionStart - tv.getSelectionEnd
    if (sel != 0)
      tv.setText("")
    else tv.getText match {
      case e: Editable =>
        if (e.length > 0)
          e.delete(e.length - 1, e.length)
        tv.setSelection(tv.getText.length, tv.getText.length)
    }
  }
  def appendText(s: String) = tweak { tv: EditText =>
    import scala.util.control.Exception.catching
    val sel = tv.getSelectionStart - tv.getSelectionEnd
    val text = if (sel == 0) tv.getText else ""

    val ok = tv.getTag match {
      case DecimalField  | DollarField =>
        val parsed = catching(classOf[Exception]) opt {
          (text + s).toDouble
        }

        ("." == s.toString && text.length == 0) || parsed.nonEmpty
      case IntegerField =>
        val parsed = catching(classOf[Exception]) opt {
          (text + s).toLong
        }
        parsed.nonEmpty
    }


    if (ok) {
      if (sel != 0)
        tv.setText(s)
      else
        tv.append(s)
      tv.setSelection(tv.getText.length, tv.getText.length)
    }
  }
  lazy val matchWidth = lp[LinearLayout](MATCH_PARENT, WRAP_CONTENT)
  lazy val wrapContent = lp2(WRAP_CONTENT, WRAP_CONTENT)  {
    p: LinearLayout.LayoutParams =>
      p.gravity = Gravity.RIGHT
    }

  lazy val inputButtonSize = 72 dp

  def margin[A <: View](left: Int = 0, top: Int = 0,
                             right: Int = 0, bottom: Int = 0,
                             all: Int = 0) = tweak { v: A =>
    val m = v.getLayoutParams.asInstanceOf[ViewGroup.MarginLayoutParams]
    if (all > 0) {
      m.topMargin    = all
      m.rightMargin  = all
      m.bottomMargin = all
      m.leftMargin   = all
    } else {
      m.topMargin    = top
      m.rightMargin  = right
      m.bottomMargin = bottom
      m.leftMargin   = left
    }
  }

  def image(resid: Int) = Tweak[ImageView](_.setImageResource(resid))
  def bg(color: Int) = Tweak[View](_.setBackgroundColor(color))

  val numberInputTweaks = tweak { e: EditText =>
    e.setMaxLines(1)
    e.setMaxEms(9)
    e.setMinEms(9)
    e.setGravity(Gravity.RIGHT)
    e.setFilters(Array(new InputFilter.LengthFilter(13)))
    e.setTextAppearance(this, android.R.style.TextAppearance_Large)
    e.setCompoundDrawablePadding(8 dp)
    e.setKeyListener(null)
    e.setTextIsSelectable(true)
    e.setFocusable(true)
    e.setFocusableInTouchMode(false)
  } + FuncOn.click { v: View =>
    selectedField = Some(v.asInstanceOf[TextView])
    val popup = new PopupWindow(this)
    popup.setContentView(getUi(inputLayout))
    val lp = popup.getContentView.getLayoutParams
    popup.setWidth(lp.width)
    popup.setHeight(lp.height)
    popup.setFocusable(true)
    popup.showAtLocation(findViewById(android.R.id.content), Gravity.CENTER, 0, 0)
    v.requestFocusFromTouch()
    popupWindow = Some(popup)

    val tw = v.getTag match {
      case DecimalField  | DollarField => show
      case IntegerField => tweak { v: View => v.setVisibility(View.INVISIBLE) }
    }
    (decimalButton <~ tw) ~
      (inputText <~ tweak { tv: EditText =>
        tv.setText(v.asInstanceOf[TextView].getText)
        tv.setSelection(0, tv.getText.length)
        tv.setTag(v.getTag)
      }) map (_ => true)
  }

  val inputButtonTweaks = tweak { b: Button =>
    b.setTextAppearance(this, android.R.style.TextAppearance_Medium)
    b.setTextColor(Color.WHITE)
  } + lp[TableRow](inputButtonSize, inputButtonSize) + FuncOn.click { v: View =>
    v.getTag match {
      case n: Integer => inputText <~ appendText(n.toString)
      case s: String => inputText <~ appendText(s)
    }
  }

  val dollarTweak = tweak { tv: TextView =>
    tv.setCompoundDrawables(TextDrawable("$"), null, null, null)
  }

  val percentTweak = tweak { tv: TextView =>
    tv.setCompoundDrawables(null, null, TextDrawable("%"), null)
  }

  val smallNumberInputTweaks = numberInputTweaks + tweak { e: EditText =>
    e.setMaxEms(4)
    e.setMinEms(4)
    e.setSelectAllOnFocus(true)
    e.setFilters(Array(new InputFilter.LengthFilter(6)))
    e.setClickable(true)
  }

  val labelTweaks = tweak { t: TextView =>
    t.setTextAppearance(this, android.R.style.TextAppearance_Medium)
  }
  val inputTextTweaks = tweak { tv: EditText =>
    tv.setInputType(InputType.TYPE_CLASS_NUMBER |
      InputType.TYPE_NUMBER_FLAG_DECIMAL)
    tv.setKeyListener(null)
    tv.setGravity(Gravity.RIGHT | Gravity.CENTER)
    tv.setSingleLine(true)
    tv.setPadding(8 dp, 0, 0, 0)
    tv.setTextAppearance(this, android.R.style.TextAppearance_Large)
    tv.setTextColor(Color.WHITE)
    tv.setText("0", TextView.BufferType.EDITABLE)
    tv.setTextIsSelectable(true)
  }

  lazy val headerPadding = padding(all = 4 dp)
  lazy val headerTweak = tweak { tv: TextView =>
    tv.setGravity(Gravity.CENTER)
    tv.setEllipsize(TruncateAt.END)
    tv.setSingleLine(true)
    tv.setPadding(4 dp, 4 dp, 4 dp, 4 dp)
  }
  lazy val headerTransform = Transformer {
    case tv: TextView => tv <~ headerTweak
  }

  lazy val rowTransform = Transformer {
    case tv: TextView =>
      tv.setGravity(Gravity.RIGHT)
      tv.setSingleLine(true)
      tv <~ padding(all = 4 dp)
  }
  lazy val amortRowTransform = Transformer {
    case tv: TextView =>
      tv.setGravity(Gravity.RIGHT)
      tv.setSingleLine(true)
      tv <~ padding(all = 4 dp)
  }

  lazy val scrollSync = new ListViewScrollSync
  lazy val amortLayout = l[LinearLayout](
    l[LinearLayout](
      w[TextView] <~ text("#") <~
        lp[LinearLayout](48 dp, WRAP_CONTENT) <~ bg(Color.LTGRAY),
      w[ListView] <~ lp[LinearLayout](48 dp, MATCH_PARENT) <~
        tweak { lv: ListView =>
          lv.setAdapter(AmortNAdapter)
          lv.setVerticalScrollBarEnabled(false)
          scrollSync.add(lv)
        }
    ) <~ vertical,
    l[HorizontalScrollView](
      l[LinearLayout](
        l[LinearLayout](
          w[TextView] <~ text("Interest Paid") <~
            lp[LinearLayout](96 dp, WRAP_CONTENT),
          w[TextView] <~ text("Principal Paid") <~
            lp[LinearLayout](96 dp, WRAP_CONTENT),
          w[TextView] <~ text("Principal Remaining") <~
            lp[LinearLayout](120 dp, WRAP_CONTENT),
          w[TextView] <~ text("Total Interest") <~
            lp[LinearLayout](120 dp, WRAP_CONTENT),
          w[TextView] <~ text("Equity") <~
            lp[LinearLayout](120 dp, WRAP_CONTENT)
        ) <~ horizontal <~ tweak { ll: LinearLayout =>
          ll.setShowDividers(LinearLayout.SHOW_DIVIDER_MIDDLE)
          ll.setDividerDrawable(getResources.getDrawable(R.drawable.vertical_divider))
          ll.setBackgroundColor(Color.LTGRAY)
          ll.setDividerPadding(0)
        } <~ lp[LinearLayout](MATCH_PARENT, WRAP_CONTENT),
        w[ListView] <~ lp[LinearLayout](MATCH_PARENT, 0, 1) <~
          tweak { lv: ListView =>
            lv.setAdapter(AmortAdapter)
            lv.setEmptyView(getUi(w[TextView] <~
              lp[LinearLayout](MATCH_PARENT, MATCH_PARENT) <~
              text("No data has been calculated")
            ))
            scrollSync.add(lv)
          }
      ) <~ vertical <~ lp[HorizontalScrollView](MATCH_PARENT, MATCH_PARENT)
    ) <~ lp[LinearLayout](0, MATCH_PARENT, 1)
  ) <~ horizontal <~ headerTransform <~ tweak { ll: LinearLayout =>
    ll.setShowDividers(LinearLayout.SHOW_DIVIDER_MIDDLE)
    ll.setDividerDrawable(getResources.getDrawable(R.drawable.vertical_divider))
    ll.setDividerPadding(0)
  }

  def createAmortRow = {
    var ipaid: TextView    = null
    var pvpaid: TextView   = null
    var pvremain: TextView = null
    var itotal: TextView   = null
    var equity: TextView   = null

    val amortRowLayout = l[LinearLayout](
      w[TextView] <~ text("Interest Paid") <~ wire(ipaid) <~
        lp[LinearLayout](96 dp, WRAP_CONTENT),
      w[TextView] <~ text("Principal Paid") <~ wire(pvpaid) <~
        lp[LinearLayout](96 dp, WRAP_CONTENT),
      w[TextView] <~ text("Principal Remaining") <~ wire(pvremain) <~
        lp[LinearLayout](120 dp, WRAP_CONTENT),
      w[TextView] <~ text("Total Interest") <~ wire(itotal) <~
        lp[LinearLayout](120 dp, WRAP_CONTENT),
      w[TextView] <~ text("Equity") <~ wire(equity) <~
        lp[LinearLayout](120 dp, WRAP_CONTENT)
    ) <~ horizontal <~ tweak { ll: LinearLayout =>
      ll.setShowDividers(LinearLayout.SHOW_DIVIDER_MIDDLE)
      ll.setDividerDrawable(getResources.getDrawable(R.drawable.vertical_divider))
      ll.setDividerPadding(0)
    } <~ amortRowTransform

    val view = getUi(amortRowLayout)

    view.setTag(AmortRow(ipaid, pvpaid, pvremain, itotal, equity))
    view
  }

  lazy val inputLayout: Ui[TableLayout] = l[TableLayout](
    l[TableRow](
      l[LinearLayout](
        w[EditText] <~ inputTextTweaks <~ bg(0) <~
          lp[LinearLayout](0, 64 dp, 1) <~ wire(inputText),
        w[InputImageButton] <~
          image(android.R.drawable.ic_menu_close_clear_cancel) <~
          lp2(48 dp, 48 dp) { p: LinearLayout.LayoutParams =>
            p.gravity = Gravity.CENTER
          } <~
          tweak { v: ImageButton =>
            v.setScaleType(ImageView.ScaleType.CENTER_INSIDE)
          } <~ On.longClick {
            inputText <~ text("") map { _ => true }
          } <~ On.click {
            inputText <~ deleteLast map { _ => true }
          }
      ) <~ lp2(MATCH_PARENT, WRAP_CONTENT) { lp: TableRow.LayoutParams =>
        lp.span = 3
      } <~
        tweak { v: View =>
        } <~ bg(Color.BLACK)
    ),
    l[TableRow](
      w[Button] <~ text("7") <~ hold(7),
      w[Button] <~ text("8") <~ hold(8),
      w[Button] <~ text("9") <~ hold(9)
    ) <~ lp[TableLayout](MATCH_PARENT, WRAP_CONTENT),
    l[TableRow](
      w[Button] <~ text("4") <~ hold(4),
      w[Button] <~ text("5") <~ hold(5),
      w[Button] <~ text("6") <~ hold(6)
    ) <~ lp[TableLayout](MATCH_PARENT, WRAP_CONTENT),
    l[TableRow](
      w[Button] <~ text("1") <~ hold(1),
      w[Button] <~ text("2") <~ hold(2),
      w[Button] <~ text("3") <~ hold(3)
    ) <~ lp[TableLayout](MATCH_PARENT, WRAP_CONTENT),
    l[TableRow](
      w[Button] <~ text(".") <~ hold(".") <~
        wire(decimalButton),
      w[Button] <~ text("0") <~ hold(0),
      w[ImageButton] <~ image(android.R.drawable.ic_menu_send) <~
        lp[TableRow](inputButtonSize, inputButtonSize) <~ On.click {
          Ui(popupWindow map { p =>
            selectedField map { case (s) =>
              val t = inputText.getText.toString
              if (t.length > 0) {
                val text = s.getTag match {
                  case DecimalField => "%.3f" format t.toDouble
                  case DollarField => "%.2f" format t.toDouble
                  case IntegerField => t
                }
                s.setText(text)
              }
            }
            selectedField = None
            p.dismiss()
          })
      }
    ) <~ lp[TableLayout](MATCH_PARENT, WRAP_CONTENT)
  ) <~ lp[FrameLayout](WRAP_CONTENT, WRAP_CONTENT) <~ Transformer {
    case b: Button => b <~ inputButtonTweaks
  }

  lazy val pagerLayout = w[HSViewPager] <~ tweak { v: ViewPager =>
    v.setFitsSystemWindows(true)
    v.setAdapter(Adapter)
  } <~ wire(viewPager)

  lazy val layout = l[ScrollView](
    l[LinearLayout](
      w[TextView] <~ labelTweaks <~ text("Principal (PV)"),
      l[LinearLayout](
        w[InputButton] <~ text("=") <~ lp[LinearLayout](48 dp, 48 dp) <~
          On.click { calculatePV() },
        w[View] <~ lp[LinearLayout](0, 0, 1),
        w[EditText] <~ wrapContent <~ numberInputTweaks <~ dollarTweak <~
          hold(DollarField) <~ wire(pvField)
      ) <~ matchWidth,
      w[TextView] <~ labelTweaks <~ text("Balance (FV)") <~ hide,
      l[LinearLayout](
        w[InputButton] <~ text("=") <~ lp[LinearLayout](48 dp, 48 dp) <~
          On.click { calculateFV() },
        w[View] <~ lp[LinearLayout](0, 0, 1),
        w[EditText] <~ wrapContent <~ numberInputTweaks <~ text("0") <~
          dollarTweak <~ hold(DollarField) <~ wire(fvField)
      ) <~ matchWidth <~ hide,
      w[TextView] <~ labelTweaks <~ text("Interest Rate (I)"),
      l[LinearLayout](
        w[InputButton] <~ text("=") <~ lp[LinearLayout](48 dp, 48 dp) <~
          On.click { calculateI() },
        w[View] <~ lp[LinearLayout](0, 0, 1),
        w[EditText] <~ wrapContent <~ smallNumberInputTweaks <~
          text("4.500") <~ percentTweak <~ hold(DecimalField) <~ wire(iField)
      ) <~ matchWidth,
      w[TextView] <~ labelTweaks <~ text("Payment (A)"),
      l[LinearLayout](
        w[InputButton] <~ text("=") <~ lp[LinearLayout](48 dp, 48 dp) <~
          On.click { calculateA() },
        w[View] <~ lp[LinearLayout](0, 0, 1),
        w[EditText] <~ wrapContent <~ numberInputTweaks <~ dollarTweak <~
          hold(DollarField) <~ wire(aField)
      ) <~ matchWidth,
      w[TextView] <~ labelTweaks <~ text("Total Payments (N)"),
      l[LinearLayout](
        w[InputButton] <~ text("=") <~ lp[LinearLayout](48 dp, 48 dp) <~
          On.click { calculateN() },
        w[View] <~ lp[LinearLayout](0, 0, 1),
        w[EditText] <~ wrapContent <~ smallNumberInputTweaks <~ text("360") <~
          hold(IntegerField) <~ wire(nField)
      ),
      w[TextView] <~ labelTweaks <~ text("N/year"),
      w[EditText] <~ wrapContent <~ smallNumberInputTweaks <~ text("12") <~
        hold(IntegerField) <~ wire(nyrField),
      w[ImageButton] <~ image(android.R.drawable.ic_media_next) <~
        lp2(WRAP_CONTENT, WRAP_CONTENT) { ll: LinearLayout.LayoutParams =>
          ll.gravity = Gravity.RIGHT
          ll.topMargin = 24 dp
        } <~ hide <~ wire(amortPageButton) <~ On.click {
          viewPager.setCurrentItem(1)
          Ui(true)
        }
    ) <~ vertical <~ padding(all = 12 dp)
  ) <~ tweak { v: View => v.setFitsSystemWindows(true) }

  override def onCreate(state: Bundle) {
    super.onCreate(state)
    getWindow.setSoftInputMode(
      WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN)
    setContentView(getUi(pagerLayout))
  }

  /**
   * Solve for P:
   * P = (A * (1 - (1 + i)**(-n)) / i)
   */
  def calculatePV() = {
    values match {
      case data@TVMValues(_,Some(fv),Some(i),Some(a),Some(n),nyr) =>
        val pi = periodRate(i, nyr)

        val r = if (i != BigDecimal(0)) a * (1 - (1 / (1 + pi).pow(n))) / pi
          else a * n

        calculateAmortization(data.copy(pv = Some(r)))
        pvField <~ text(
          r.setScale(2, BigDecimal.RoundingMode.HALF_UP).toString())
      case _ =>
        toast("Cannot calculate present value with missing fields") <~ fry
    }
  }

  def calculateFV() = {
    values match {
      case TVMValues(Some(pv),_,Some(i),Some(a),Some(n),nyr) =>
        toast("Not implemented") <~ fry
      case _ =>
        toast("Cannot calculate future value with missing fields") <~ fry
    }
  }

  /**
   * Approximate for i, since i is not solvable in the above.  We will
   * use Newton-Raphson approximation.
   * <p>
   * These following equations are taking the above amortization formula,
   * and solving for zero, then taking the derivative to achieve f'(x)
   * </p>
   * <pre>
   * f(x) =  (P*i*((1 + i)**n)) - (A*(((1 + i)**n) - 1))
   * Simplified:
   * f(x) = (((1 + i)**n)*((P*i) - A)) + A
   *
   *                          n*((P*i) - A)
   * f'(x) = ((1 + i)**n)*(P + -------------)
   *                             (1 + i)
   * </pre>
   * Newton-Raphson approximation is of the form:
   * <pre>
   *                   f(x[y])
   * x[y+1] = x[y] - ----------
   *                  f'(x[y])
   * </pre>
   * We repeatedly calculate this until the difference between x[y+1] and
   * x[y] is within margin.
   */
  def calculateI() = {
    val ESTIMATE_START   = 0.0000
    val ESTIMATE_INCR    = 0.0005
    val ESTIMATE_END     = 0.1666
    val ESTIMATE_DIVERGE = 0.1
    val ESTIMATE_MARGIN  = 0.0000000001

    @tailrec
    def doRNInterestApproximation(estimate: Double, previous: Double,
                                  n: Int, pv: Double,
                                  a: Double): Double = {
      import scala.math._

      if (estimate > 0 && (estimate - previous).abs < ESTIMATE_DIVERGE) {
        val last = estimate
        val f = pow(1 + estimate, n) * (pv * estimate - a) + a
        val f1 = pow(1 + estimate, n) *
          (n * (pv * estimate - a) / (1 + estimate) + pv)

        if (f1 == 0) {
          -1
        } else {

          val next = estimate - f / f1

          if ((next - last).abs < ESTIMATE_MARGIN)
            next
          else
            doRNInterestApproximation(next, last, n, pv, a)
        }
      } else -1
    }
    values match {
      case data@TVMValues(Some(pv),Some(fv),_,Some(a),Some(n),nyr) =>
        val range = Range.Double(ESTIMATE_START, ESTIMATE_END, ESTIMATE_INCR)

        val result = range.scanLeft(0.0) { (ac,b) =>
          if (ac > 0) {
            ac
          } else {
            val estimate = doRNInterestApproximation(
              b, b, n, pv.doubleValue(), a.doubleValue())
            if (estimate > 0) estimate * nyr * 100 else ac
          }
        }.dropWhile(_ == 0).headOption

        result map { r =>
          calculateAmortization(data.copy(i = Some(r)))
          iField <~ text("%.3f" format r)
        } getOrElse (
          toast("Unable to solve interest rate") <~ fry
        )
      case _ =>
        toast("Cannot calculate interest rate with missing fields") <~ fry
    }
  }

  /**
   * Basic amortization equations:
   * <pre>
   * A = P * ( (i * (1 + i)**n) / ((1 + i)**n - 1) )
   * A = (P * i) / (1 - (1 + i)**(-n))
   * </pre>
   */
  def calculateA() = {
    values match {
      case data@TVMValues(Some(pv),Some(fv),Some(i),_,Some(n),nyr) =>
        val pi = periodRate(i, nyr)
        val r = if (i != BigDecimal(0))
          (pv * pi) / (1 -  1 / (1 + pi).pow(n))
        else
          pv / n

        calculateAmortization(data.copy(a = Some(r)))
        aField <~ text(r.setScale(
          2, BigDecimal.RoundingMode.UP).toString())
      case _ =>
        toast("Cannot calculate payment with missing fields") <~ fry
    }
  }

  /**
   * Solve for n:
   * n = -(log(1 - (P * i)/A) / log(1 + i))
   */
  def calculateN() = {
    import math._
    values match {
      case data@TVMValues(Some(pv),Some(fv),Some(i),Some(a),_,nyr) =>
        val pi = periodRate(i, nyr)
        val r = if (i == BigDecimal(0)) (pv / a).doubleValue else
          -1 * (log((1 - (pv * pi / a)).doubleValue()) /
            log((1 + pi).doubleValue()))
        calculateAmortization(data.copy(n = Some(ceil(r).toInt)))
        nField <~ text(ceil(r).toInt.toString)
      case _ =>
        toast("Cannot calculate number of payments with missing fields") <~ fry
    }
  }

  @tailrec
  private def _calculateAmortization(n: Int, rate: Double,
                                     remaining: Double, a: Double,
                                     itotal: Double, ptotal: Double,
                                     acc: List[AmortRowData] = Nil):
                                     List[AmortRowData] = {
    if (remaining > 0) {
      val ipaid = remaining * rate
      val ppaid = math.min(a - ipaid, remaining)
      _calculateAmortization(
        n + 1, rate, remaining - ppaid, a, itotal + ipaid, ptotal + ppaid,
        AmortRowData(n, ipaid, ppaid, remaining - ppaid, itotal + ipaid,
          ptotal + ppaid) :: acc)
    } else {
      acc
    }
  }

  def calculateAmortization(data: TVMValues) = data match {
    case TVMValues(Some(pv),_,Some(i),Some(a),_,nyr) =>
      val rate = periodRate(i, nyr).doubleValue()
      val rows = Vector(_calculateAmortization(
        1, rate, pv.doubleValue(), a.doubleValue(), 0, 0).reverse: _*)
      AmortAdapter.data = rows
      AmortAdapter.notifyDataSetChanged()
      AmortNAdapter.data = rows
      AmortNAdapter.notifyDataSetChanged()
      Adapter.calculatedAmortization()
    case _ => getUi(toast(
      "Cannot amortize without principal, interest and payment") <~ fry)
  }


  override def onBackPressed(): Unit = {
    if (viewPager.getCurrentItem == 0)
      super.onBackPressed()
    else
      viewPager.setCurrentItem(0)

  }

  object AmortNAdapter extends BaseAdapter {
    var data = Seq.empty[AmortRowData]

    override def getCount = data.length

    override def getItemId(p1: Int) = p1

    override def getView(pos: Int, convert: View, p3: ViewGroup) = {
      if (convert != null) {
        convert.asInstanceOf[TextView].setText(data(pos).n.toString)
        convert
      } else
        getUi(w[TextView] <~ text(data(pos).n.toString) <~ headerTweak <~
          tweak { tv: TextView => tv.setGravity(Gravity.RIGHT) } )
    }

    override def getItem(pos: Int) = data(pos)
  }

  object AmortAdapter extends BaseAdapter {
    var data = Seq.empty[AmortRowData]
    override def getCount = data.length

    override def getItemId(p1: Int) = p1

    override def getView(pos: Int, convert: View, container: ViewGroup) = {
      val view = if (convert == null) createAmortRow else convert
      val holder = view.getTag.asInstanceOf[AmortRow]
      val d = data(pos)
      holder.ipaid.setText("$%.2f" format d.ipaid)
      holder.equity.setText("$%.2f" format d.equity)
      holder.iTotal.setText("$%.2f" format d.iTotal)
      holder.pvremain.setText("$%.2f" format d.pvremain)
      holder.pvpaid.setText("$%.2f" format d.pvpaid)
      view
    }

    override def getItem(pos: Int) = data(pos)
  }

  object Adapter extends PagerAdapter {
    private var amortCalculated = false

    def calculatedAmortization() {
      amortCalculated = true
      getUi(amortPageButton <~ show)
      notifyDataSetChanged()
    }

    override def getCount = if (amortCalculated) 2 else 1

    override def isViewFromObject(p1: View, p2: scala.Any) = p1 == p2

    override def instantiateItem(container: ViewGroup, position: Int) = {
      val v = position match {
        case 0 =>
          val v = getUi(layout)
          loadDefaults()
          v
        case 1 =>
          getUi(amortLayout)
      }

      container.addView(v)
      v
    }
  }
}

case class TextDrawable(text: String)(implicit c: AppContext) extends Drawable {

  val textPaint = new TextPaint
  textPaint.setTypeface(Typeface.SANS_SERIF)

  textPaint.setColor(Color.BLACK)
  textPaint.setTextSize(14 sp)
  textPaint.setAntiAlias(true)
  textPaint.setFakeBoldText(true)
  textPaint.setStyle(Paint.Style.FILL)
  textPaint.setTextAlign(Paint.Align.LEFT)

  val bounds = new Rect
  textPaint.getTextBounds(text, 0, text.length, bounds)

  override def draw(canvas: Canvas) =
      canvas.drawText(text, -bounds.width/2, bounds.height / 2, textPaint)

  override def getIntrinsicHeight = bounds.height
  override def getIntrinsicWidth = bounds.width
  override def setColorFilter(f: ColorFilter) = textPaint.setColorFilter(f)
  override def setAlpha(p1: Int) = textPaint.setAlpha(p1)
  override def getOpacity = PixelFormat.TRANSLUCENT
}

class InputImageButton(c: Context) extends ImageButton(
  c, null, android.R.attr.buttonBarButtonStyle)
class InputButton(c: Context) extends Button(
  c, null, android.R.attr.buttonBarButtonStyle)

class HSViewPager(c: Context) extends ViewPager(c) with AutoLogTag {
  override def canScroll(v: View, checkV: Boolean, dx: Int, x: Int, y: Int) =
    v match {
      case h: HorizontalScrollView => dx < 0 || h.getScrollX > 0
      case _ => super.canScroll(v, checkV, dx, x, y)
    }
}

class ListViewScrollSync {
  var lists = Set.empty[ListView]
  var touchSource = Map.empty[MotionEvent,View]
  var clickSource: View = _

  def add(l: ListView): Unit = {
    lists += l

    l.setOnTouchListener(new OnTouchListener() {
      override def onTouch(v: View, event: MotionEvent) = {
        if (touchSource.get(event).isEmpty)
          touchSource += event -> v

        if (touchSource(event) == v) {
          (lists - v.asInstanceOf[ListView]) foreach {
            _.dispatchTouchEvent(event)
          }

          if (event.getAction == MotionEvent.ACTION_UP) {
            clickSource = v
          }
          touchSource -= event
        }

        false
      }
    })
    l.setOnScrollListener(new OnScrollListener() {
      override def onScroll(view: AbsListView, first: Int, visCount: Int, totalCount: Int) {
        if (view == clickSource)
          (lists - view.asInstanceOf[ListView]) foreach {
            _.setSelectionFromTop(first, view.getChildAt(0).getTop)
          }
      }
      override def onScrollStateChanged(p1: AbsListView, p2: Int) = ()
    })
  }
}
