import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Random;
import java.util.Scanner;

public class HumanPlayer extends Player {
	Random r = new Random();

	@Override
	public void doTurn() {
		char ans;
		Scanner scanner = new Scanner(System.in);
		while (true) {
			System.out.print(name + ", do you want to take new card (y/n)? ");
			ans = scanner.next().charAt(0);
			if (ans == 'y')
				return;
			else if (ans == 'n') {
				stopPlaying();
				return;
			} else System.out.println(name + ", are you retarded?");
		}
	}
}
