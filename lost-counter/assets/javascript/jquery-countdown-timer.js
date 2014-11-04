$.fn.extend({
    countdownTimer: function () {
        var self = $.fn.countdownTimer,
            options = $.extend({}, self.defaultOptions, options);

        $(this).each(function(i, element) {
            self.init($(element), options);
        });

        var pad = function (n, width) {
            n = n + '';
            return n.length >= width ? n : new Array(width - n.length + 1).join('0') + n;
        }

        return {
            changeTime: function(value) {
                // Add zero padding
                value = pad(value, 3)

                self.changeDigit(0, value[0]);
                self.changeDigit(1, value[1]);
                self.changeDigit(2, value[2]);

                self.play();
            },

            reset: function() {
                self.changeDigit(0, "0");
                self.changeDigit(1, "0");
                self.changeDigit(2, "0");
            }
        }
    }
});

$.fn.extend($.fn.countdownTimer, {

    defaultOptions: {},

    buildDigitTemplate: function(value) {
        var template = "<li>" +
                            "<a href=\"#\">" +
                                "<div class=\"up\">" +
                                    "<div class=\"shadow\"></div>" +
                                    "<div class=\"inn\">" + value + "</div>" +
                                "</div>" +
                                "<div class=\"down\">" +
                                    "<div class=\"shadow\"></div>" +
                                    "<div class=\"inn\">" + value + "</div>" +
                                "</div>" +
                            "</a>" +
                        "</li>";

        return template
    },

    buildDigitTemplateGroup: function(value) {
        var template = $("<ul class=\"flip\">" + this.buildDigitTemplate(value) + "</ul>");
        return template;
    },

    changeDigit: function(index, newValue) {
        var $element,
            $firstElement,
            oldValue;

        $element = $('body').find('ul.flip').eq(index);
        oldValue = $element.find('.inn').eq(0).text();

        if(oldValue == newValue) { return; }

        $firstElement = $element.children('li').eq(0);
        $secondElement = $(this.buildDigitTemplate(newValue));
        $element.append($secondElement);

        $firstElement.addClass("before")
                .removeClass("active")
                .next("li")
                .addClass("active");

        setTimeout(function() {
            $firstElement.remove();
        }, 500);
    },

    play: function() {
        $('body').addClass("play");
    },

    init: function($element, options) {
        self = this;

        var $secondDigits = [self.buildDigitTemplateGroup(0), self.buildDigitTemplateGroup(0), self.buildDigitTemplateGroup(0)];

        $.each($secondDigits, function(index, elem) {
            $element.append(elem);
        });
    }
});