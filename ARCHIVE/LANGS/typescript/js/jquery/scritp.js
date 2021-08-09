$(function () {
  let link1 = $('<a href="http://www.google.com">link</a>');
  console.log("making link1");

  // properties.
  // it's like getprop in list
  let link2 = $('<a></a>');
  link2.prop("href", "https://www.google.com");
  link2.prop("title", "title...");
  link2.html("content go here");
  console.log("making link2");


  let link3 = $('<a>', {
    href: "https://www.google.com",
    title: "jQuery",
    text: "Visit us"
  }
  );

  console.log("making link3");

  console.log("registering events");

  $("#button1").on("click", function () {
    alert("good");
    $(".meh").prop("checked", !$(".meh").prop("checked"));
  });

  // events
  $("#menu").hide();
  $("#button2").on("click", function () {
    console.log("good2");
    $("#menu").slideDown(500);
  });
  $("#button2").on("mouseleave", function () {
    console.log("good2");
    $("#menu").slideUp(500);
  });

  $("#button3").on("click", function () {
    console.log("good3");
    $("#menu").animate({left: '200px'});
  });

  let anchor = document.getElementById("anchor");
  console.log(anchor);
  anchor.appendChild(link1);
  anchor.appendChild(link2);
  anchor.appendChild(link3);

  function dismissNotification() {
    $("#notification").fadeOut(500);
  }

  $("#notificationBtn").on("click", function () {
    console.log("notify button");
    $("#notification").animate({
      right: "0px",
      opacity: "1"
    }, 500)
    .animate({
      top: 0
    });
    window.setTimeout(function() {
      dismissNotification();
    }, 3000);
  })
});

