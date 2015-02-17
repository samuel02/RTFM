"use strict";

$(document).ready(function() {

  var $connectButton = $("#connect-btn"),
      $disconnectButton = $("#disconnect-btn"),
      $textInput = $("#text-input"),
      $timerContainer = $("#timer"),
      $inputContainer = $('.input-container'),
      url = "ws://localhost:5000",
      socket;

  // Init timer and status bar
  console.log("Not connected.");
  $disconnectButton.attr("disabled", "disabled");
  var $timer = $('#timer').countdownTimer();

  $connectButton.click(function() {
    $(this).attr("disabled", "disabled");

    socket = new WebSocket(url, 'lost-protocol');

    socket.addEventListener("open", function() {
      $disconnectButton.removeAttr("disabled");
      console.log("Connected.");
      $inputContainer.fadeIn();
    });

    socket.addEventListener("message", function(event) {
      var value = Number.parseInt(event.data)

      if (value >= 0) {
        if(value <= 90) blink();
        $timer.changeTime(value);
      } else {
        socket.close();
        alert("SYSTEM FAILURE!");
      }

    });

    socket.addEventListener("error", function(event) {
      console.error(event.data);
    });

    socket.addEventListener("close", function(event) {
      $connectButton.removeAttr("disabled");
      $disconnectButton.attr("disabled", "disabled");
      $timer.reset();
      $inputContainer.fadeOut();
      console.log("Disconnected.");
    });
  });

  $disconnectButton.on("click", function(event) {
    $(this).attr("disabled", "disabled");
    $timer.reset();
    socket.close();
  });

  $textInput.keypress(function (e) {
    var value;

    if (e.which == 13) {
      value = $(this).val();
      if(socket) sendData(value);
      $(this).val("");
    }
  });

  function sendData(data) {
    socket.send(data);
  }

  function blink() {
    $('.blink').addClass('blinking');

    setTimeout(function() {
      $('.blink').removeClass('blinking');
    }, 800)
  }
});