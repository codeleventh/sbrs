import exceptions.GameException;
import exceptions.OutOfCardsException;
import sun.io.Win32ErrorMode;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Type;
import java.util.*;

public class GameManager {
	DealerPlayer dealer;
	List<Player> players, winners = new ArrayList<>();
	Deck deck;

	Double bank = 0d;
	final Integer BLACKJACK = 52, DECKSIZE = 12, BET = 40;

	GameManager(List<Player> players, Deck deck) {
		this.players = players;
		this.deck = deck; // получаем колоду извне — для её сохранения между партиями

		// правим возможный некорректный ввод
		if(!hasHumanPlayers())
			players.add(new HumanPlayer());
		for(Player p : players)
			if(p instanceof DealerPlayer) {
				dealer = (DealerPlayer) p;
				break;
			}
		if(dealer == null) {
			dealer = new DealerPlayer();
			players.add(dealer);
		}
	}
	void play() throws GameException {
		// reinit
		if(deck.size() < DECKSIZE / 3)
			deck = Deck.getNormalDeck();
		winners = new ArrayList<>();

		for(int i = 0; i < players.size(); i++) {
			if(players.get(i).money < BET) {
				System.out.println(players.get(i).name + " is a bankrupt and out of game.");
				players.remove(i--);
				if(!hasHumanPlayers())
					throw new GameException("There are no human players anymore. Game over.");
			} else {
				players.get(i).money -= BET;
				bank += BET;
				System.out.println(players.get(i).name + " is in game and have $" + players.get(i).money);
			}
		}
		System.out.println("The bank is $" + bank);

		try {
			throwCards(false);
			throwCards(false);

			for(Player player : players)
				if(player.getScore() == BLACKJACK)
					winners.add(player);

			// is instant win?
			if(winners.size() > 0) {
				if(winners.size() > 1 && winners.contains(dealer))
					winners.remove(dealer);
				finalGame();
				return;
			}

			while(hasIngamePlayers()) {
				for(Player player : players) {
					if(player.inGame)
						player.doTurn();
				}
				throwCards(true);
			}
			while(dealer.inGame) {
				dealer.doTurn();
				throwCards(false);
			}
		} catch(OutOfCardsException e) {
			System.out.println("The deck is empty now. Checking for winners.");
			selectWinners();
			finalGame();
			return;
		}
		selectWinners();
		finalGame();
	}

	private void selectWinners() {
		// случай, когда instant winners отсутствуют
		players.sort((p1, p2) -> p1.getScore() - p2.getScore());
		// некрасиво:
		Integer max = -1;
		for(Integer i = 0; i < players.size(); i++) {
			if(players.get(i).getScore() > 21)
				continue;
			else max = players.get(i).getScore();
		}
		for(Integer i = 0; i < players.size(); i++) {
			if(players.get(i).getScore() == max)
				winners.add(players.get(i));
		}
	}
	private void finalGame() {
		// подсчет очков и награждение победителей
		String s = new String();
		if(winners.size() > 0) {
			for(Player winner : winners) {
				winner.money += bank / winners.size();
				s += winner.name + ", ";
			} s = s.substring(0, s.length() - 2);
			if(winners.size() == 1)
				System.out.println("The winner is " + s + ". He's got $" + bank + ".");
			else if(winners.size() > 1)
				System.out.println("The winners is: " + s + ". Each of them got $" + bank / winners.size() + ".");
		} else
			System.out.println("Lol, everybody loose.");
		return;
	}

	private void throwCards(boolean ignoreDealer) throws OutOfCardsException {
		Card c;
		for(Player player : players) {
			if(player == dealer && ignoreDealer)
				continue;
			c = deck.getRandomCard();
			player.takeCard(c);
		}
	}

	private Boolean hasHumanPlayers() {
		for(Player player : players)
			if(player instanceof HumanPlayer)
				return true;
		return false;
	}
	private Boolean hasIngamePlayers() {
		for(Player player : players)
			if(player.inGame == true && player != dealer)
				return true;
		return false;
	}

	public static void main(String[] args) {
		ArrayList<Player> initList = new ArrayList<>(); // нельзя arrays.aslist - он не даст добавлять элементы
		initList.add(new DealerPlayer());
		initList.add(new HumanPlayer());
		initList.add(new HumanPlayer());
		initList.add(new PcPlayer());

		GameManager gm_old, gm = new GameManager(initList, Deck.getNormalDeck());
		char ans;
		Scanner scanner = new Scanner(System.in);
		while(true) {
			System.out.print("Do you wanna play (y/n)? ");
			ans = scanner.next().charAt(0);
			if(ans == 'n') {
				System.out.print("Bye!");
				return;
			} else if(ans == 'y') {
				try {
					{// new game
						gm_old = gm;
						gm = new GameManager(gm_old.players, gm_old.deck);
						for(Player player : gm.players)
							player.reset();
					}
					gm.play();
				} catch(GameException e) {
					System.out.println(e);
					continue;
				}
			} else {
				System.out.println("Are you retarted?");

			}
		}
	}
}
